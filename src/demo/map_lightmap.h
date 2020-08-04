#pragma once

////////////////////////////////////////////////////////////////

namespace Demo {
	namespace Lightmap {
		/* Lighting parameters */
		constexpr u16
			TexelSize		= 16,
			Dilate			= true,
			JitterOccluded	= false,
			NumEnvSamples	= 12
		;

		static constexpr vec3 Ambient = {
			2.f * 6.25f,
			2.f * 6.25f,
			2.f * 7.f,
		};

		/* Debugging */
		enum class DebugMode {
			Off,
			Tiles,
			Checker,
			Positions,
			Normals,
		};
		constexpr DebugMode Debug = DebugMode::Off;

		constexpr float
			PointScale		= 7500.f * 2.f,
			BounceScale		= 16.f,
			ThreshIgnore	= 2.f,
			SurfaceBias		= 4.f,
			EnvRayLength	= 8192.f
		;

		/* Metadata */
		constexpr auto
			Descriptor		= Texture::Descriptors[Texture::Lightmap];

		constexpr u16
			Width			= Descriptor.width,
			Height			= Descriptor.height;

		constexpr u32
			TexelCount		= Width * Height;

		////////////////////////////////////////////////////////////////

		u32 PackVec3(const vec3& v) {
			return 
				clamp((i32)floor(v.x * 255.f + 0.5f), 0, 255) << 16 |
				clamp((i32)floor(v.y * 255.f + 0.5f), 0, 255) <<  8 |
				clamp((i32)floor(v.z * 255.f + 0.5f), 0, 255) <<  0 ;
		}

		void UnpackVec3(vec3& v, u32 u) {
			v.x = (u >> 16) & 255;
			v.y = (u >>  8) & 255;
			v.z = (u >>  0) & 255;
		}
	} // namespace Lightmap
} // namespace Demo

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::AllocLightmap() {
	using namespace Demo;

	lightmap.data = Mem::Alloc<u32>(Lightmap::TexelCount);
#ifdef ENABLE_RADIOSITY
	lightmap.bounce_data = Mem::Alloc<u32>(Lightmap::TexelCount);
#endif
	lightmap.pos = Mem::Alloc<vec3>(Lightmap::TexelCount);
	lightmap.nor = Mem::Alloc<vec3>(Lightmap::TexelCount);
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::Details::PackLightmap() {
	using namespace Demo;

	MemSet(lightmap.pos, 0, Lightmap::TexelCount);
	MemSet(lightmap.nor, 0, Lightmap::TexelCount);

	lightmap.packer.Init(Lightmap::Width, Lightmap::Height);

	/* pack patches */
	for (u16 patch_index = 0; patch_index < patches.count; ++patch_index) {
		PackedMap::Patch patch;
		source->GetPatch(patch, patches.GetSourceIndex(patch_index));
		bool is_mirrored = patches.IsMirrored(patch_index);

		u16 num_control_points = patch.width * patch.height;

		const u16 MaxPatchVertices = 64;
		PackedMap::PatchVertex ctrl[MaxPatchVertices];

		assert(num_control_points <= MaxPatchVertices);
		for (u16 i = 0; i < num_control_points; ++i) {
			source->GetPatchVertex(ctrl[i], patch, i + patches.control_start[patch_index]);
			if (is_mirrored) {
				ctrl[i].pos[symmetry_axis] = 2 * symmetry_level - ctrl[i].pos[symmetry_axis];
			}
		}

		vec2 size = 0.f;
		for (u16 j = 0; j < patch.height; j += 2) {
			const PackedMap::PatchVertex* row = ctrl + j * patch.width;
			float width = 0.f;
			for (u16 i = 2; i < patch.width; i += 2)
				width += length(row[i].pos - row[i - 2].pos);
			if (size.x < width)
				size.x = width;
		}

		for (u16 i = 0; i < patch.width; i += 2) {
			const PackedMap::PatchVertex* column = ctrl + i;
			float height = 0.f;
			for (u16 j = 2; j < patch.height; j += 2, column += 2 * patch.width)
				height += length(column[2 * patch.width].pos - column[0].pos);
			if (size.y < height)
				size.y = height;
		}

		size.x = ceil(size.x / Lightmap::TexelSize);
		size.y = ceil(size.y / Lightmap::TexelSize);

		auto tile = lightmap.packer.Add(u16(size.x) + 1, u16(size.y) + 1);
		assert(tile != lightmap.packer.Full);
		auto& rect = lightmap.packer.GetTile(tile);

		for (u16 vtx_index = patches.vertex_start[patch_index], vtx_end = vtx_index + patches.vertex_count[patch_index]; vtx_index < vtx_end; ++vtx_index) {
			vec2& lightmap_uv = texcoords[vtx_index].zw;
			lightmap_uv.x = (rect.min[0] + 0.5f + lightmap_uv.x * size.x) / Lightmap::Width;
			lightmap_uv.y = (rect.min[1] + 0.5f + lightmap_uv.y * size.y) / Lightmap::Height;
		}
		
		vec3* texel_pos = lightmap.pos + rect.min[1] * Lightmap::Width + rect.min[0];
		vec3* texel_nor = lightmap.nor + rect.min[1] * Lightmap::Width + rect.min[0];

		for (u16 y = 0, height = rect.GetHeight(); y < height; ++y, texel_pos += Lightmap::Width, texel_nor += Lightmap::Width) {
			float t = y / float(height - 1);

			for (u16 x = 0, width = rect.GetWidth(); x < width; ++x) {
				float s = x / float(width - 1);

				vec2 uv;
				EvaluatePatch(patch, ctrl, s, t, texel_pos[x], texel_nor[x], uv);
				
				u32 flip_sign = is_mirrored << 31;
				*(u32*)&texel_nor[x][0] ^= flip_sign;
				*(u32*)&texel_nor[x][1] ^= flip_sign;
				*(u32*)&texel_nor[x][2] ^= flip_sign;
			}
		}
	}

	/* pack brush planes */
	for (u16 brush_index = 0; brush_index < brushes.count; ++brush_index) {
		u32 end_plane_index = brushes.start[brush_index + 1];
		for (u32 plane_index = brushes.start[brush_index]; plane_index < end_plane_index; ++plane_index) {
			auto material = brushes.GetPlaneMaterial(plane_index);
			auto props = Material::Properties[material];
			auto vis = props & Material::MaskVisibility;
			if (vis != Material::Opaque)
				continue;

			auto vtx_range = brushes.plane_vertex_range[plane_index];
			auto vtx_count = vtx_range.GetCount();
			if (vtx_count < 3)
				continue;
		
			auto vtx_offset = vtx_range.GetOffset();
			vec3* pos = positions + vtx_offset;
			vec4* uv = texcoords + vtx_offset;

			u8 uv_axis = brushes.GetPlaneUVAxis(plane_index);
			u8 s_axis = brushes.GetSAxis(uv_axis);
			u8 t_axis = brushes.GetTAxis(uv_axis);

			Rect uv_bounds;
			uv_bounds.clear();
			for (vec3 *v = pos, *endv = pos + vtx_count; v != endv; ++v)
				uv_bounds.add(vec2{(*v)[s_axis], (*v)[t_axis]});

			uv_bounds.mins.x = floor(uv_bounds.mins.x / Lightmap::TexelSize) - 0.5f;
			uv_bounds.mins.y = floor(uv_bounds.mins.y / Lightmap::TexelSize) - 0.5f;
			uv_bounds.maxs.x = ceil(uv_bounds.maxs.x / Lightmap::TexelSize) + 0.5f;
			uv_bounds.maxs.y = ceil(uv_bounds.maxs.y / Lightmap::TexelSize) + 0.5f;

			const vec4& plane = brushes.planes[plane_index];
			auto uv_unmap = [&](float s, float t) -> vec3 {
				s = (s + uv_bounds.mins.x + 0.5f) * Lightmap::TexelSize;
				t = (t + uv_bounds.mins.y + 0.5f) * Lightmap::TexelSize;

				vec3 pos;
				pos[s_axis ] = s;
				pos[t_axis ] = t;
				pos[uv_axis] = -(plane.w + s * plane[s_axis] + t * plane[t_axis]) / plane[uv_axis];

				if (0) {
					vec2 uv;
					uv.x = pos[s_axis] / Lightmap::TexelSize;
					uv.y = pos[t_axis] / Lightmap::TexelSize;

					assert(uv.x >= uv_bounds.mins.x);
					assert(uv.x <= uv_bounds.maxs.x);
					assert(uv.y >= uv_bounds.mins.y);
					assert(uv.y <= uv_bounds.maxs.y);
				}

				return pos;
			};

			auto tile = lightmap.packer.Add(u16(uv_bounds.width()), u16(uv_bounds.height()));
			assert(tile != lightmap.packer.Full);
			auto& rect = lightmap.packer.GetTile(tile);

			vec2& lightmap_offset = brushes.plane_lmap_offset[plane_index];
			lightmap_offset[0] = uv_bounds.mins[0] - rect.min[0];
			lightmap_offset[1] = uv_bounds.mins[1] - rect.min[1];

			for (u16 vtx_index = 0; vtx_index < vtx_count; ++vtx_index) {
				vec2& lightmap_uv = uv[vtx_index].zw;
				vec3& p = pos[vtx_index];
				lightmap_uv.x = (p[s_axis] / Lightmap::TexelSize - lightmap_offset[0]) / Lightmap::Width;
				lightmap_uv.y = (p[t_axis] / Lightmap::TexelSize - lightmap_offset[1]) / Lightmap::Height;
			}

			vec3* texel_pos = lightmap.pos + rect.min[1] * Lightmap::Width + rect.min[0];
			vec3* texel_nor = lightmap.nor + rect.min[1] * Lightmap::Width + rect.min[0];

			for (u16 y = 0, height = rect.GetHeight(); y < height; ++y, texel_pos += Lightmap::Width, texel_nor += Lightmap::Width) {
				for (u16 x = 0, width = rect.GetWidth(); x < width; ++x) {
					vec3 pos = uv_unmap(x, y);

					if (0) {
						const float MaxBorder = Lightmap::TexelSize;

						float edge_dist = 0.f;
						for (u16 i = brushes.start[brush_index]; i < end_plane_index; ++i) {
							if (i == plane_index)
								continue;
							const vec4& other_plane = brushes.planes[i];
							assign_max(edge_dist, dot(other_plane.xyz, pos) + other_plane.w);
						}

						if (edge_dist > MaxBorder)
							continue;
					}

					texel_pos[x] = pos;
					texel_nor[x] = plane.xyz;
				}
			}
		}
	}
}

////////////////////////////////////////////////////////////////

namespace Details {
	namespace ParallelFor {
		struct Batch {
			u32 begin;
			u32 end;
			void* data;
			void (*work)(u32 begin, u32 end, void* data);
		};
	
		void Thunk(void* param) {
			Batch* batch = (Batch*)param;
			batch->work(batch->begin, batch->end, batch->data);
		};
	}
}

void ParallelFor(u32 count, void* data, void (*work)(u32 begin, u32 end, void* data)) {
	if (!count)
		return;

	const u32 MAX_NUM_THREADS = 256; // should be enough for TR5

	u32 cpu_threads = Sys::GetNumCPUThreads();
	if (cpu_threads > MAX_NUM_THREADS)
		cpu_threads = MAX_NUM_THREADS;
	if (cpu_threads > count)
		cpu_threads = count;
	u32 batch_size = (count + cpu_threads - 1) / cpu_threads;

	using Batch		= Details::ParallelFor::Batch;
	Batch			batches[MAX_NUM_THREADS];
	Sys::Thread		workers[MAX_NUM_THREADS];

	for (u32 batch_index = 0, offset = 0; batch_index < cpu_threads; ++batch_index, offset += batch_size) {
		auto& batch = batches[batch_index];
		batch.begin = offset;
		batch.end = min(offset + batch_size, count);
		batch.data = data;
		batch.work = work;
		auto& thread = workers[batch_index];
		thread.work = Details::ParallelFor::Thunk;
		thread.data = &batch;
		Sys::SpawnThread(thread);
	}

	for (u32 batch_index = 0; batch_index < cpu_threads; ++batch_index)
		Sys::JoinThread(workers[batch_index]);
}

////////////////////////////////////////////////////////////////

NOINLINE void ComputeTangentFrame(const vec3& z, vec3& x, vec3& y) {
	// find normal component with smallest magnitude
	u32 next_axis = 0;
	for (u32 i = 1; i < 2; ++i)
		if (abs(z[i]) < abs(z[next_axis]))
			next_axis = i;

	// setup tangent frame
	MemSet(&x);
	x[next_axis] = 1.f;
	cross(z, x, y);
	normalize(y);
	cross(z, y, x);
}

////////////////////////////////////////////////////////////////

// http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/
namespace R2 {
	static constexpr float
		G = 1.324718,
		X = 1.f / G,
		Y = 1.f / (G * G);

	struct CosineHemisphere {
		float u = 0.5f;
		float v = Math::PI;

		vec3 NextSample() {
			float cos_theta = sqrt(1.f - fract(u));
			float sin_theta = sqrt(1.f - cos_theta * cos_theta);
			float phi = v;

			u += R2::X;
			v += R2::Y * Math::TAU;

			return {
				sin_theta * cos(phi),
				sin_theta * sin(phi),
				cos_theta
			};
		}
	};
} // namespace R2

////////////////////////////////////////////////////////////////

NOINLINE void Map::Details::SampleLighting(const vec3& pos, const vec3& nor, const vec3& x_axis, const vec3& y_axis, TraceInfo& trace, vec3& accum, LightGrid::InfluenceList* influences, LightMode mode) {
	using namespace Demo;

	if (mode != LightMode::Bounce) {
		for (u16 light_index = 0; light_index < Map::num_lights; ++light_index) {
			const auto& light = Map::lights[light_index];
			vec3 light_pos = light.position;
			if (light_index == 0)
				light_pos += pos;

			vec3 light_dir = pos - light_pos;
			float dist = length(light_dir);
			float angle;
			if (!influences) {
				angle = -dot(nor, light_dir);
				if (angle < 0.f)
					continue;
				if (dist > 0.f)
					angle /= dist;
			} else {
				angle = 1.f;
			}

			assign_max(dist, 16.f);

			float scale = light.intensity * angle;
			if (light_index != 0)
				scale *= Lightmap::PointScale / (dist * dist);
			if (scale < Lightmap::ThreshIgnore)
				continue;

			if (light.flags & Light::IsSpotlight) {
				if (!EnableSpotlights)
					continue;
				float dist_by_normal = dot(light_dir, light.spot.xyz);
				if (dist_by_normal < 0.f)
					continue;

				const float Radius = 64.f;
				float radius_by_dist = (Radius + 16.f) / light.spot.w;
				float radius_at_dist = radius_by_dist * dist_by_normal;
				vec3 point_at_dist = light.position;
				mad(point_at_dist, light.spot.xyz, dist_by_normal);
				float sample_radius = length(pos - point_at_dist);
				if (sample_radius >= radius_at_dist)
					continue;

				if (sample_radius > radius_at_dist - 32.f)
					scale *= (radius_at_dist - sample_radius) * (1.f/32.f);
			}

			if (mode == LightMode::Shadows) {
				// offset sampling point towards the light
				vec3 delta = light_pos - pos;
				float dist = length(delta);
				float bias = Lightmap::SurfaceBias / dist;
				trace.start.x = pos.x + delta.x * bias;
				trace.start.y = pos.y + delta.y * bias;
				trace.start.z = pos.z + delta.z * bias;
				trace.delta.x = light_pos.x - trace.start.x;
				trace.delta.y = light_pos.y - trace.start.y;
				trace.delta.z = light_pos.z - trace.start.z;

				bool hit = Map::TraceRay(trace);
				if (light_index == 0) {
					using namespace Demo::Material;
					if (!hit || GetVisibility(brushes.GetPlaneMaterial(trace.plane)) != Sky)
						continue;
				} else if (hit) {
					continue;
				}
			}

			mad(accum, light.color, scale);
			if (influences && influences->count < size(influences->data)) {
				LightGrid::Influence& inf = influences->data[influences->count++];
				mul(inf.color, light.color, scale);
				inf.dir = -light_dir;
			}
		}
	}

	if (mode == LightMode::Bounce || (mode == LightMode::Shadows && source->skylight)) {
		vec3 skylight;
		skylight.r = ((source->skylight      ) & 255) / float(Lightmap::NumEnvSamples);
		skylight.g = ((source->skylight >>  8) & 255) / float(Lightmap::NumEnvSamples);
		skylight.b = ((source->skylight >> 16) & 255) / float(Lightmap::NumEnvSamples);

		R2::CosineHemisphere hemisphere;
		for (u16 i = 0; i < Lightmap::NumEnvSamples; ++i) {
			trace.start.x = pos.x + nor.x;
			trace.start.y = pos.y + nor.y;
			trace.start.z = pos.z + nor.z;

			vec3 dir = hemisphere.NextSample();

			mul(trace.delta, x_axis, Lightmap::EnvRayLength * dir.x);
			mad(trace.delta, y_axis, Lightmap::EnvRayLength * dir.y);
			mad(trace.delta, nor,    Lightmap::EnvRayLength * dir.z);

			if (!Map::TraceRay(trace))
				continue;

			using namespace Demo::Material;
			auto vis = GetVisibility(brushes.GetPlaneMaterial(trace.plane));

			if (mode == LightMode::Shadows) {
				if (vis == Sky) {
					accum += skylight;
					if (influences && influences->count < size(influences->data)) {
						LightGrid::Influence& inf = influences->data[influences->count++];
						inf.color = skylight;
						inf.dir = trace.delta;
					}
				}
#ifdef ENABLE_RADIOSITY
			} else {
				if (vis != Opaque)
					continue;

				u8 uv_axis = brushes.GetPlaneUVAxis(trace.plane);
				u8 s_axis = brushes.GetSAxis(uv_axis);
				u8 t_axis = brushes.GetTAxis(uv_axis);

				const vec2& lightmap_offset = brushes.plane_lmap_offset[trace.plane];
				i32 x = clamp(i32(trace.hit_point[s_axis] / Lightmap::TexelSize - lightmap_offset[0] - 0.5f), 0, Lightmap::Width - 1);
				i32 y = clamp(i32(trace.hit_point[t_axis] / Lightmap::TexelSize - lightmap_offset[1] - 0.5f), 0, Lightmap::Height - 1);

				vec3 sample;
				Lightmap::UnpackVec3(sample, lightmap.bounce_data[y * Lightmap::Width + x]);

				float dist = max(16.f, trace.fraction * Lightmap::EnvRayLength);
				float scale = Lightmap::BounceScale / Lightmap::NumEnvSamples;
				scale /= dist;

				mad(accum, sample, min(scale, 0.25f));
#endif
			}
		}
	}
}

NOINLINE void Map::Details::InitLightTrace(TraceInfo& trace) {
	MemSet(&trace);
	trace.type = TraceInfo::Type::Lightmap;
	// prevent the ray from squeezing between diagonally-adjacent brushes
	// this fixes a sun light leak in the left hallway of DM1
	trace.box_half_size = 1.f/32.f;
}

// Maintains hue instead of clamping to white
NOINLINE u32 Map::Details::ClampColor(const vec3& accum) {
	float max_value = 255.f;
	for (u8 i = 0; i < 3; ++i)
		if (max_value < accum[i])
			max_value = accum[i];
	float scale = 255.f / max_value;

	u32 result = 0;
	for (u8 i = 0; i < 3; ++i)
		result = (result << 8) | i32(accum[i] * scale);

	return result;
}

////////////////////////////////////////////////////////////////

void Map::ComputeLighting(LightMode mode) {
	using namespace Demo;

#ifdef DEV
	if (mode == LightMode::Shadows)
		mode = LightMode::Draft;
#endif

	if constexpr (Lightmap::Debug == Lightmap::DebugMode::Off) {
#ifdef ENABLE_RADIOSITY
		if (mode == LightMode::Bounce) {
			Swap(lightmap.data, lightmap.bounce_data);
			MemSet(lightmap.data, 0, Lightmap::TexelCount);
		}
#endif // ENABLE_RADIOSITY

		struct Params {
			LightMode mode;
		} params{mode};

		ParallelFor(Lightmap::Height, &params, [](u32 y, u32 yend, void* data) {
			Params* params = (Params*)data;

			u32* texel = Map::lightmap.data + y * Lightmap::Width;
			vec3* texel_pos = Map::lightmap.pos + y * Lightmap::Width;
			vec3* texel_nor = Map::lightmap.nor + y * Lightmap::Width;

			TraceInfo trace;
			Details::InitLightTrace(trace);

			TraceInfo occlusion_trace;
			if constexpr (Lightmap::JitterOccluded) {
				MemCopy(&occlusion_trace, &trace);
			}

			for (; y < yend; ++y) {
				for (u16 x = 0; x < Lightmap::Width; ++x, ++texel_pos, ++texel_nor, ++texel) {
					vec3 pos = *texel_pos;
					const vec3& nor = *texel_nor;

#ifdef ENABLE_RADIOSITY
					vec3 accum;
					if (params->mode == LightMode::Bounce) {
						Lightmap::UnpackVec3(accum, lightmap.bounce_data[texel - lightmap.data]);
					} else {
						accum = Lightmap::Ambient;
					}
#else
					vec3 accum = Lightmap::Ambient;
#endif // ENABLE_RADIOSITY

					if (length_squared(nor) > 0.f) {
						vec3 x_axis, y_axis;
						ComputeTangentFrame(nor, x_axis, y_axis);

						if constexpr (Lightmap::JitterOccluded)
							Details::GetUnoccludedPos(pos, nor, x_axis, y_axis, occlusion_trace);

						Details::SampleLighting(pos, nor, x_axis, y_axis, trace, accum, nullptr, params->mode);
					}

					u32 color = Details::ClampColor(accum);
					if (length_squared(nor) > 0.f)
						color |= 0xff00'0000;

					*texel = color;
				}
			}
		});

		if (Lightmap::Dilate) {
			for (u16 tile_index = 0, tile_count = lightmap.packer.GetNumTiles(); tile_index < tile_count; ++tile_index) {
				auto& rect = lightmap.packer.GetTile(tile_index);
				/* propagate right/down */
				u32* texel = lightmap.data + rect.min[1] * Lightmap::Width;
				for (u16 y = rect.min[1]; y < rect.max[1]; ++y, texel += Lightmap::Width) {
					for (u16 x = rect.min[0]; x < rect.max[0]; ++x) {
						if (x > rect.min[0] && !(texel[x] >> 24))
							texel[x] = texel[x - 1];
						if (y > rect.min[1] && !(texel[x] >> 24))
							texel[x] = texel[x - Lightmap::Width];
					}
				}
				/* propagate left/up */
				texel = lightmap.data + (rect.max[1] - 1) * Lightmap::Width;
				for (u16 y = rect.max[1]; y > rect.min[1]; --y, texel -= Lightmap::Width) {
					for (u16 x = rect.max[0]; x > rect.min[0]; --x) {
						if (x < rect.max[0] && !(texel[x - 1] >> 24))
							texel[x - 1] = texel[x];
						if (y < rect.max[1] && !(texel[x - 1] >> 24))
							texel[x - 1] = texel[x + Lightmap::Width - 1];
					}
				}
			}
		}
	} else {
		Details::DebugFillLightmap();
	}

	/* compute vertex lighting (for models) */

	if (mode != LightMode::Bounce) {
		ParallelFor(num_model_vertices, &mode, [](u32 begin, u32 end, void* data) {
			LightMode mode = *(LightMode*)data;

			TraceInfo trace;
			Details::InitLightTrace(trace);

			for (u32 i = begin; i < end; ++i) {
				u32 index = Map::model_vertex_indices[i];
				const vec3& pos = Map::positions[index];
				const vec3& packed_nor = Map::normals[index];
				vec3 nor;
				Demo::Model::UnpackNormal(packed_nor, nor);
				u32& color = Map::colors[index];

				if (length_squared(nor) > 0.f) {
					vec3 x_axis, y_axis;
					ComputeTangentFrame(nor, x_axis, y_axis);
			
					vec3 accum = Lightmap::Ambient;
					Details::SampleLighting(pos, nor, x_axis, y_axis, trace, accum, nullptr, mode);
					color = Details::ClampColor(accum);
				} else {
					color = 0;
				}
			}
		});
	}

	/* compute model light grid */

	MemSet(&lightgrid);

	u32 num_grid_points = 1;
	for (u32 axis = 0; axis < 3; ++axis) {
		u32 size_bits = LightGrid::GridSizeBits[axis];
		u32 grid_size = LightGrid::GridSize[axis];
		lightgrid.offset[axis] = i32(Map::brushes.world_bounds[0][axis]) & ~(grid_size - 1);
		//if (axis != 2)
		//	lightgrid.offset[axis] += grid_size / 2;
		lightgrid.dims[axis] = (Map::brushes.world_bounds[1][axis] - lightgrid.offset[axis] + (grid_size - 1)) >> size_bits;
		num_grid_points *= lightgrid.dims[axis];
		assert(num_grid_points <= LightGrid::MaxPoints);
	}

	ParallelFor(num_grid_points, &mode, [](u32 begin, u32 end, void* data) {
		LightMode mode = *(LightMode*)data;

		TraceInfo trace;
		Details::InitLightTrace(trace);

		for (; begin < end; ++begin) {
			u32 i = begin;

			u32 cell[3];
			cell[0] = i % lightgrid.dims[0]; i /= lightgrid.dims[0];
			cell[1] = i % lightgrid.dims[1]; i /= lightgrid.dims[1];
			cell[2] = i;

			vec3 pos;
			for (u32 axis = 0; axis < 3; ++axis)
				pos[axis] = i32(lightgrid.offset[axis]) + i32(cell[axis] * LightGrid::GridSize[axis]);

			LightGrid::Point& sample = lightgrid.points[begin];
			LightGrid::InfluenceList influences;
			influences.count = 0;

			vec3 ignore_color;
			Details::SampleLighting(pos, i4x4.GetAxis(2), i4x4.GetAxis(0), i4x4.GetAxis(1), trace, ignore_color, &influences, mode);

			for (u32 j = 0; j < influences.count; ++j) {
				LightGrid::Influence& inf = influences.data[j];
				safe_normalize(inf.dir);
				mad(sample.dir, inf.dir, length(inf.color));
			}
			safe_normalize(sample.dir);

			for (u32 j = 0; j < influences.count; ++j) {
				LightGrid::Influence& inf = influences.data[j];
				float align = max(0.f, dot(inf.dir, sample.dir));
				mad(sample.color, inf.color, align);
				mad(sample.ambient, inf.color, 0.25f * (1.f - align));
			}

			mad(sample.ambient, sample.color, 0.125f);

			Details::ClampColor(sample.color);
			Details::ClampColor(sample.ambient);
			sample.color /= 255.f / 2.f; // normalize + overbright
			sample.ambient /= 255.f / 2.f; // normalize + overbright
		}
	});

#ifdef SAVE_LIGHTGRID
	{
		u32 width = lightgrid.dims[0];
		u32 height = lightgrid.dims[1] * lightgrid.dims[2];
		u32 num_pixels = width * height;
		u32* pixels = Sys::Alloc<u32>(num_pixels);

		static constexpr const char* Paths[] = {"grid_color.tga", "grid_ambient.tga", "grid_dir.tga",};

		for (u32 channel = 0; channel < size(Paths); ++channel) {
			for (u32 z = 0, idx = 0; z < lightgrid.dims[2]; ++z) {
				for (u32 y = 0; y < lightgrid.dims[1]; ++y) {
					for (u32 x = 0; x < lightgrid.dims[0]; ++x, ++idx) {
						switch (channel) {
							case 0 : pixels[idx] = Lightmap::PackVec3(lightgrid.points[idx].color); break;
							case 1 : pixels[idx] = Lightmap::PackVec3(lightgrid.points[idx].ambient); break;
							case 2 : pixels[idx] = Lightmap::PackVec3(lightgrid.points[idx].dir * 0.5f + 0.5f); break;
							default: pixels[idx] = 0x00'FF'00'FF; break;
						}
						pixels[idx] |= 0xFF'00'00'00;
					}
				}
			}
			Gfx::SaveTGA(Paths[channel], pixels, width, height);
		}

		Sys::Free(pixels);
	}
#endif
}

////////////////////////////////////////////////////////////////
// Test if sampling position is occluded and, if so, compute
// an average unoccluded position.
//
// Trace needs to be pre-filled (at least type = Lightmap)
////////////////////////////////////////////////////////////////
FORCEINLINE void Map::Details::GetUnoccludedPos(vec3& pos, const vec3& nor, const vec3& x_axis, const vec3& y_axis, TraceInfo& trace) {
	using namespace Demo;

	trace.start = pos + nor;
	trace.delta = nor;

	if (Map::TraceRay(trace)) {
		const u16 Divs = 4;
		const float
			Scale =  Lightmap::TexelSize / float(Divs),
			Bias  = -Lightmap::TexelSize * (0.5f - 0.5f / Divs)
		;

		vec4 average_offset;
		MemSet(&average_offset);

		for (u16 i = 0; i < Divs * Divs; ++i) {
			vec3 offset = nor;
			mad(offset, x_axis, (i % Divs) * Scale + Bias);
			mad(offset, y_axis, (i / Divs) * Scale + Bias);

			trace.start.x = pos.x + offset.x;
			trace.start.y = pos.y + offset.y;
			trace.start.z = pos.z + offset.z;

			if (!Map::TraceRay(trace)) {
				average_offset.xyz += offset;
				++average_offset.w;
			}
		}

		if (average_offset.w)
			mad(pos, average_offset.xyz, 1.f / average_offset.w);
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::Details::DebugFillLightmap() {
	using namespace Demo;

	for (u16 tile_index = 0, tile_count = lightmap.packer.GetNumTiles(); tile_index < tile_count; ++tile_index) {
		auto& rect = lightmap.packer.GetTile(tile_index);

		u32* texel = lightmap.data + rect.min[1] * Lightmap::Width;
		vec3* texel_pos = lightmap.pos + rect.min[1] * Lightmap::Width;
		vec3* texel_nor = lightmap.nor + rect.min[1] * Lightmap::Width;

		for (u16 y = rect.min[1]; y < rect.max[1]; ++y, texel += Lightmap::Width, texel_pos += Lightmap::Width, texel_nor += Lightmap::Width) {
			for (u16 x = rect.min[0]; x < rect.max[0]; ++x) {
				switch (Lightmap::Debug) {
					case Lightmap::DebugMode::Tiles:
						texel[x] = tile_index * 0x45d9f3b;
						break;

					case Lightmap::DebugMode::Checker:
						texel[x] = (x ^ y) & 1 ? 0xffff'ffff : 0x3f3f'3f3f;
						break;

					case Lightmap::DebugMode::Positions:
						texel[x] = Lightmap::PackVec3({
							fract(texel_pos[x].x / 64.f),
							fract(texel_pos[x].y / 64.f),
							fract(texel_pos[x].z / 64.f)
						});
						break;

					case Lightmap::DebugMode::Normals:
						texel[x] = Lightmap::PackVec3(texel_nor[x] * 0.5f + 0.5f);
						break;

					default:
						texel[x] = 0xFF00FF;
						break;
				}
			}
		}
	}
}

////////////////////////////////////////////////////////////////

NOINLINE void Map::UpdateLightmapTexture() {
	Gfx::SetTextureContents(Demo::Texture::Lightmap, lightmap.data);
	Gfx::UploadGeometry(&Map::colors[0], Map::num_total_vertices, Gfx::Arena::Level, Map::gpu_addr.colors);
#ifdef SAVE_LIGHTMAP
	Gfx::SaveTGA("lightmap.tga", Demo::Texture::Lightmap);
#endif
}
