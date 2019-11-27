#pragma once

////////////////////////////////////////////////////////////////

namespace Demo {
	namespace Lightmap {
		/* Lighting parameters */
		constexpr float
			PointScale		= 7500.f * 6.f,
			ThreshIgnore	= 4.f
		;

		/* Metadata */
		constexpr auto
			Descriptor		= Texture::Descriptors[Texture::Lightmap];

		constexpr u16
			Width			= Descriptor.width,
			Height			= Descriptor.height,
			Scale			= 16,
			Dilate			= true;

		/* Debugging */
		enum class DebugMode {
			Off,
			Tiles,
			Checker,
			Positions,
			Normals,
		};
		constexpr DebugMode Debug = DebugMode::Off;

		constexpr u32 PackVec3(const vec3& v) {
			return 
				clamp((i32)floor(v.x * 255.f + 0.5f), 0, 255) << 16 |
				clamp((i32)floor(v.y * 255.f + 0.5f), 0, 255) <<  8 |
				clamp((i32)floor(v.z * 255.f + 0.5f), 0, 255) <<  0 ;
		}
	} // namespace Lightmap
} // namespace Demo

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::InitLightmap() {
	using namespace Demo;

	lightmap.data = Mem::Alloc<u32>(Lightmap::Width * Lightmap::Height);
	lightmap.pos = Mem::Alloc<vec3>(Lightmap::Width * Lightmap::Height);
	lightmap.nor = Mem::Alloc<vec3>(Lightmap::Width * Lightmap::Height);

	MemSet(lightmap.pos, 0, sizeof(vec3) * Lightmap::Width * Lightmap::Height);
	MemSet(lightmap.nor, 0, sizeof(vec3) * Lightmap::Width * Lightmap::Height);

	lightmap.packer.Init(Lightmap::Width, Lightmap::Height);

	/* pack patches */
	for (u16 patch_index = 0; patch_index < patches.count; ++patch_index) {
		auto patch = source->GetPatch(patches.GetSourceIndex(patch_index));
		u16 num_control_points = patch.width * patch.height;

		const u16 MaxPatchVertices = 64;
		PackedMap::PatchVertex ctrl[MaxPatchVertices];

		assert(num_control_points <= MaxPatchVertices);
		for (u16 i = 0; i < num_control_points; ++i)
			ctrl[i] = source->GetPatchVertex(i + patches.control_start[patch_index]);

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

		size.x = ceil(size.x / Lightmap::Scale);
		size.y = ceil(size.y / Lightmap::Scale);

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

		u8 prim_x = patch.width >> 1;
		u8 prim_y = patch.height >> 1;

		for (u16 y = 0, height = rect.GetHeight(); y < height; ++y, texel_pos += Lightmap::Width, texel_nor += Lightmap::Width) {
			float t = y * (prim_y / float(height - 1));
			i16 j = min((i16)floor(t), prim_y - 1);
			t -= j;
			j <<= 1;
			assert(j + 2 < patch.height);
			float t_coeff[3] = {(1.f - t) * (1.f - t), 2.f * t * (1.f - t), t * t};
			PackedMap::PatchVertex* row = ctrl + j * patch.width;

			for (u16 x = 0, width = rect.GetWidth(); x < width; ++x) {
				float s = x * (prim_x / float(width - 1));
				i16 i = min((i16)floor(s), prim_x - 1);
				s -= i;
				i <<= 1;
				assert(i + 2 < patch.width);
				float s_coeff[3] = {(1.f - s) * (1.f - s), 2.f * s * (1.f - s), s * s};

				vec3 v[3];
				auto* c = row + i;
				for (u16 k = 0; k < 3; ++k, c += patch.width) {
					v[k].x = s_coeff[0]*c[0].pos.x + s_coeff[1]*c[1].pos.x + s_coeff[2]*c[2].pos.x;
					v[k].y = s_coeff[0]*c[0].pos.y + s_coeff[1]*c[1].pos.y + s_coeff[2]*c[2].pos.y;
					v[k].z = s_coeff[0]*c[0].pos.z + s_coeff[1]*c[1].pos.z + s_coeff[2]*c[2].pos.z;
				}

				texel_pos[x].x = t_coeff[0]*v[0].x + t_coeff[1]*v[1].x + t_coeff[2]*v[2].x;
				texel_pos[x].y = t_coeff[0]*v[0].y + t_coeff[1]*v[1].y + t_coeff[2]*v[2].y;
				texel_pos[x].z = t_coeff[0]*v[0].z + t_coeff[1]*v[1].z + t_coeff[2]*v[2].z;

				vec3 dt = t_coeff[0] * (v[1] - v[0]) + t_coeff[2] * (v[2] - v[1]);

				c = row + i;
				for (u16 k = 0; k < 3; ++k, ++c) {
					v[k].x = t_coeff[0]*c[0].pos.x + t_coeff[1]*c[patch.width].pos.x + t_coeff[2]*c[patch.width*2].pos.x;
					v[k].y = t_coeff[0]*c[0].pos.y + t_coeff[1]*c[patch.width].pos.y + t_coeff[2]*c[patch.width*2].pos.y;
					v[k].z = t_coeff[0]*c[0].pos.z + t_coeff[1]*c[patch.width].pos.z + t_coeff[2]*c[patch.width*2].pos.z;
				}
				vec3 ds = s_coeff[0] * (v[1] - v[0]) + s_coeff[2] * (v[2] - v[1]);

				safe_normalize(cross(dt, ds), texel_nor[x]);
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
			u8 s_axis = (uv_axis == 0);			// 1 0 0
			u8 t_axis = (uv_axis != 2) + 1;		// 2 2 1

			Rect uv_bounds;
			uv_bounds.clear();
			for (vec3 *v = pos, *endv = pos + vtx_count; v != endv; ++v)
				uv_bounds.add(vec2{(*v)[s_axis], (*v)[t_axis]});

			uv_bounds.mins.x = floor(uv_bounds.mins.x / Lightmap::Scale) - 0.5f;
			uv_bounds.mins.y = floor(uv_bounds.mins.y / Lightmap::Scale) - 0.5f;
			uv_bounds.maxs.x = ceil(uv_bounds.maxs.x / Lightmap::Scale) + 0.5f;
			uv_bounds.maxs.y = ceil(uv_bounds.maxs.y / Lightmap::Scale) + 0.5f;

			const vec4& plane = brushes.planes[plane_index];
			auto uv_unmap = [&](float s, float t) -> vec3 {
				s = (s + uv_bounds.mins.x + 0.5f) * Lightmap::Scale;
				t = (t + uv_bounds.mins.y + 0.5f) * Lightmap::Scale;

				vec3 pos;
				pos[s_axis ] = s;
				pos[t_axis ] = t;
				pos[uv_axis] = -(plane.w + s * plane[s_axis] + t * plane[t_axis]) / plane[uv_axis];

				if (0) {
					vec2 uv;
					uv.x = pos[s_axis] / Lightmap::Scale;
					uv.y = pos[t_axis] / Lightmap::Scale;

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

			for (u16 vtx_index = 0; vtx_index < vtx_count; ++vtx_index) {
				vec2& lightmap_uv = uv[vtx_index].zw;
				vec3& p = pos[vtx_index];
				lightmap_uv.x = (p[s_axis] / Lightmap::Scale - uv_bounds.mins.x + rect.min[0]) / Lightmap::Width;
				lightmap_uv.y = (p[t_axis] / Lightmap::Scale - uv_bounds.mins.y + rect.min[1]) / Lightmap::Height;
			}

			vec3* texel_pos = lightmap.pos + rect.min[1] * Lightmap::Width + rect.min[0];
			vec3* texel_nor = lightmap.nor + rect.min[1] * Lightmap::Width + rect.min[0];
				
			for (u16 y = 0, height = rect.GetHeight(); y < height; ++y, texel_pos += Lightmap::Width, texel_nor += Lightmap::Width) {
				for (u16 x = 0, width = rect.GetWidth(); x < width; ++x) {
					vec3 pos = uv_unmap(x, y);

					if (0) {
						const float MaxBorder = Lightmap::Scale;

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

void Map::ComputeLighting(bool shadows) {
	using namespace Demo;

#ifdef DEV
	shadows = false;
#endif

	if constexpr (Lightmap::Debug == Lightmap::DebugMode::Off) {
		struct Params {
			Map* map;
			bool shadows;
		} params{this, shadows};

		//for (u16 y = 0; y < Lightmap::Height; ++y) {
		ParallelFor(Lightmap::Height, &params, [](u32 y, u32 yend, void* data) {
			Params* params = (Params*)data;
			Map* map = params->map;

			u32* texel = map->lightmap.data + y * Lightmap::Width;
			vec3* texel_pos = map->lightmap.pos + y * Lightmap::Width;
			vec3* texel_nor = map->lightmap.nor + y * Lightmap::Width;

			TraceInfo trace;

			for (; y < yend; ++y) {
				for (u16 x = 0; x < Lightmap::Width; ++x, ++texel_pos, ++texel_nor, ++texel) {
					vec3 pos = *texel_pos;
					const vec3& nor = *texel_nor;
					pos += nor;

					constexpr vec3 Ambient = {
						4.f * 6.25f,
						4.f * 6.25f,
						4.f * 7.f,
					};
					vec3 accum = Ambient;

					if (length_squared(nor) > 0.f) {
						for (u16 light_index = 0; light_index < map->num_lights; ++light_index) {
							const auto& light = map->lights[light_index];
							vec3 light_pos = light.position;
							if (EnableSunLight && light_index == 0)
								light_pos += pos;
							vec3 light_dir = pos - light_pos;
							float angle = -dot(nor, light_dir);
							if (angle < 0.f)
								continue;
							float dist = length(light_dir);
							if (dist > 0.f)
								angle /= dist;
							assign_max(dist, 16.f);
							float scale = light.intensity * angle;
							if (!EnableSunLight || light_index != 0)
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

							if (params->shadows) {
								trace.SetLightmap(light_pos, pos);
								// if the trace almost made it to its destination, accept it anyway
								// this eliminates some dark edges/splotchy corners
								const float ShadowTolerance = 2.f;
								if (map->TraceRay(trace) && length_squared(pos - trace.hit_point) >= ShadowTolerance * ShadowTolerance)
									continue;
							}

							mad(accum, light.color, scale);
						}
					}

					if (!params->shadows)
						accum *= 0.75f;

					u32 color = 
						min((i32)accum.x, 255) << 16 | 
						min((i32)accum.y, 255) <<  8 | 
						min((i32)accum.z, 255) <<  0 ;
					
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
}

FORCEINLINE void Map::UpdateLightmapTexture() {
	Gfx::SetTextureContents(Demo::Texture::Lightmap, lightmap.data);
}
