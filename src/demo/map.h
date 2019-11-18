#pragma once

#include "material.h"

////////////////////////////////////////////////////////////////

struct PackedMap {
	enum {
		EnableSpotlights = false,
	};

	u16					num_brushes;
	u16					num_unaligned_planes;
	u16					num_planes;
	u16					num_nonaxial_brushes;
	u16					num_patches;
	u16					num_patch_verts;
	u8					num_uvs;
	u8					num_materials;
	u8					num_lights;
	u8					num_spotlights;

	const i16*			world_bounds;
	const i16*			brush_bounds;
	const i32*			plane_data;
	const u16*			nonaxial_counts;
	const u8*			plane_materials;
	const float*		uv_data;
	const u8*			plane_uvs;
	const u16*			patches;
	const float*		patch_vertices;
	const i16*			light_data;

	template
	<
		int NumBrushBoundEntries, int NumPlaneEntries, int NumNonaxialBrushes,
		int NumUVEntries, int NumPlaneUVEntries, int NumMaterialEntries,
		int NumPatches, int NumPatchVertEntries,
		int NumLightEntries
	>
	constexpr PackedMap
	(
		const i16	(&world_bounds)		[6],
		const i16	(&brush_bounds)		[NumBrushBoundEntries],
		const i32	(&plane_data)		[NumPlaneEntries],
		const u16	(&nonaxial_counts)	[NumNonaxialBrushes],
		u8								num_materials,
		const u8	(&plane_materials)	[NumMaterialEntries],
		const float (&uv_data)			[NumUVEntries],
		const u8	(&plane_uvs)		[NumPlaneUVEntries],
		const u16	(&patches)			[NumPatches],
		const float	(&patch_verts)		[NumPatchVertEntries],
		const i16	(&light_data)		[NumLightEntries],
		u8								num_spotlights
	) :
		world_bounds			(world_bounds),
		brush_bounds			(brush_bounds),
		num_brushes				(NumBrushBoundEntries / 6),
		plane_data				(plane_data),
		num_unaligned_planes	(NumPlaneEntries / 2),
		nonaxial_counts			(nonaxial_counts),
		num_nonaxial_brushes	(NumNonaxialBrushes),
		uv_data					(uv_data),
		num_uvs					(NumUVEntries / 5),
		plane_uvs				(plane_uvs),
		plane_materials			(plane_materials),
		num_planes				(NumMaterialEntries),
		num_patches				(NumPatches),
		patches					(patches),
		patch_vertices			(patch_verts),
		num_patch_verts			(NumPatchVertEntries / 5),
		num_materials			(num_materials),
		light_data				(light_data),
		num_lights				((NumLightEntries - num_spotlights * 3) / 5),
		num_spotlights			(num_spotlights)
	{
		static_assert(NumPlaneUVEntries == NumMaterialEntries);
	}

	struct UV {
		vec2	offset;
		vec2	scale;
		float	angle;
	};

	struct Patch {
		u8		width;
		u8		height;
		u8		divx;
		u8		divy;
		u8		material;
	};

	struct PatchVertex {
		vec3	pos;
		vec2	uv;
	};

	struct Light {
		enum Flags {
			IsSpotlight = 1 << 0,
		};

		vec3	position;
		u32		flags;
		vec3	color;
		float	intensity;
		vec4	spot;
	};

	UV							GetPlaneUV(u32 plane_index) const;
	Patch						GetPatch(u32 patch_index) const;
	PatchVertex					GetPatchVertex(u32 vertex_index) const;
	void						GetLight(u32 light_index, Light& light) const;
};

////////////////////////////////////////////////////////////////

FORCEINLINE PackedMap::UV PackedMap::GetPlaneUV(u32 plane_index) const {
	const float* data = uv_data + plane_uvs[plane_index];

	UV uv;
	uv.offset.x	= *data; data += num_uvs;
	uv.offset.y	= *data; data += num_uvs;
	uv.angle	= *data; data += num_uvs;
	uv.scale.x	= *data; data += num_uvs;
	uv.scale.y	= *data;

	uv.angle	*= -Math::DEG2RAD;

	return uv;
}

NOINLINE PackedMap::Patch PackedMap::GetPatch(u32 patch_index) const {
	u16 data = patches[patch_index];

	Patch patch;
	patch.width		= ((data & 7) << 1) + 3;
	patch.height	= (((data >> 3) & 7) << 1) + 3;
	patch.divx		= 1 << ((data >> 6) & 7);
	patch.divy		= 1 << ((data >> 9) & 7);
	patch.material	= data >> 12;

	return patch;
}

NOINLINE PackedMap::PatchVertex PackedMap::GetPatchVertex(u32 vertex_index) const {
	const float* data = patch_vertices + vertex_index;

	PatchVertex v;
	v.pos.x		= *data; data += num_patch_verts;
	v.pos.y		= *data; data += num_patch_verts;
	v.pos.z		= *data; data += num_patch_verts;
	v.uv.x		= *data; data += num_patch_verts;
	v.uv.y		= *data;

	return v;
}

FORCEINLINE void PackedMap::GetLight(u32 light_index, Light& light) const {
	const i16* data = light_data + light_index * 5;

	light.position.x	= data[0];
	light.position.y	= data[1];
	light.position.z	= data[2];
	light.intensity		= data[3];
	light.color.x		= ((data[4] >>  0) & 31) / 31.f;
	light.color.y		= ((data[4] >>  5) & 31) / 31.f;
	light.color.z		= ((data[4] >> 10) & 31) / 31.f;

	u8 first_spotlight = num_lights - num_spotlights;
	light_index -= first_spotlight;
	if (EnableSpotlights) {
		if (light_index < num_spotlights) {
			data = light_data + num_lights * 5 + light_index * 3;
		
			light.flags		= Light::IsSpotlight;
			light.spot.x	= data[0];
			light.spot.y	= data[1];
			light.spot.z	= data[2];
			light.spot.w	= length(light.spot.xyz);
			light.spot.xyz /= light.spot.w;
		} else {
			light.flags		= 0;
			light.spot		= 0.f;
		}
	} else {
		light.flags = (light_index < num_spotlights) ? Light::IsSpotlight : 0;
	}
}

////////////////////////////////////////////////////////////////

namespace Demo {
	static constexpr u8 MaterialShaders[] = {
		#define PP_DEMO_MATERIAL_SHADER(path, shader, texture, contents, draw, light)		Demo::Shader::##shader,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_SHADER)
		#undef PP_DEMO_MATERIAL_SHADER
	};

	static constexpr u8 MaterialTextures[] = {
		#define PP_DEMO_MATERIAL_TEXTURE(path, shader, texture, contents, draw, light)		Demo::Texture::##texture,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_TEXTURE)
		#undef PP_DEMO_MATERIAL_TEXTURE
	};

	static_assert(size(MaterialTextures) <= 64, "Can't fit material + UV axis in 8 bits");
}

////////////////////////////////////////////////////////////////

struct Map {
	enum {
		MAX_NUM_VERTS		= 16 * 1024,
		MAX_NUM_TRIS		= 16 * 1024,
		MAX_NUM_INDICES		= MAX_NUM_TRIS * 3,
		MAX_NUM_MATERIALS	= 256,
		
		MAX_NUM_BRUSHES		= 1024,
		MAX_NUM_PLANES		= 8192,

		MAX_NUM_PATCHES		= 128,
		
		MAX_NUM_NODES		= MAX_NUM_BRUSHES,
		MIN_NODE_BRUSHES	= 1,

		MAX_NUM_LIGHTS		= 256,
	};

	enum {
		EnableSpotlights	= PackedMap::EnableSpotlights,
		EnableAreaLights	= false,
		EnableSunLight		= true,
		SnapVertices		= true,
		RecomputePlanes		= false,	// broken
	};

	// HACK: hardcoded! Should be read from packed map
	enum {
		symmetry_level		= 64,
		symmetry_axis		= 1,
	};

	u8						num_materials;

	struct {
		u16					count;
		u16					plane_count;
		i16					bounds				[MAX_NUM_BRUSHES][2][3];
		u16					start				[MAX_NUM_BRUSHES];
		vec4				planes				[MAX_NUM_PLANES];
		u8					plane_mat_uv_axis	[MAX_NUM_PLANES];	// material: 6, uv_axis: 2

		u8					GetPlaneMaterial(u16 plane_index) const { return plane_mat_uv_axis[plane_index] >> 2; }
		u8					GetPlaneUVAxis(u16 plane_index) const { return plane_mat_uv_axis[plane_index] & 3; }

		struct {
			u32				data;
			void			Set(u32 offset, u8 count)	{ data = (offset << 8) | count; }
			u32				GetOffset() const			{ return data >> 8; }
			u32				GetCount() const			{ return data & 255; }
		}					plane_vertex_range	[MAX_NUM_PLANES];
	}						brushes;

	struct {
		u16					count;
		u8					source[MAX_NUM_PATCHES];
		u16					control_start[MAX_NUM_PATCHES];
		u16					vertex_start[MAX_NUM_PATCHES];
		u16					vertex_count[MAX_NUM_PATCHES];

		u16					GetSourceIndex(u16 patch_index) const { return source[patch_index] >> 1; }
		bool				IsMirrored(u16 patch_index) const { return source[patch_index] & 1; }
	}						patches;

	struct Partition {
		struct Node {
			i16				data[2];	// leaf: [begin, end];	node: [-first_child, axis]
			i16				clip[2];	// leaf: ignored;		node: [left_clip, right_clip]

			bool			IsLeaf() const				{ return data[0] >= 0; }
			u16				GetLeftChild() const		{ return -data[0]; }
			u16				GetRightChild() const		{ return -data[0] + 1; }
			u16				GetChild(bool second) const { return -data[0] + second; }
		};

		Node				nodes[MAX_NUM_NODES];
		u16					brushes[MAX_NUM_BRUSHES];
		u16					num_nodes;
	}						partition;

	vec3					positions			[MAX_NUM_VERTS];
	vec4					texcoords			[MAX_NUM_VERTS];
	vec3					normals				[MAX_NUM_VERTS];
	u16						indices				[MAX_NUM_INDICES];
	u32						num_mat_verts		[MAX_NUM_MATERIALS];
	u32						mat_vertex_offset	[MAX_NUM_MATERIALS];
	u32						num_mat_indices		[MAX_NUM_MATERIALS];
	u32						mat_index_offset	[MAX_NUM_MATERIALS];

	const PackedMap*		source;

	struct {
		u32*				data;
		vec3*				pos;
		vec3*				nor;
		RectPacker			packer;
	}						lightmap;

	void					Load(const PackedMap& packed);

	struct TraceInfo {
		enum Type : u8 {
			Collision,
			Bullet,
			Lightmap,

			Count,
		};

		vec3				start;
		vec3				delta;
		vec3				box_half_size;
		float				fraction;
		Type				type;
		i16					plane;
		bool				start_solid;
		vec3				hit_point;
		vec3				hit_normal;

		void				SetBullet(const vec3& a, const vec3& b);
		void				SetLightmap(const vec3& a, const vec3& b);
		void				SetCollision(const vec3& a, const vec3& travel, const vec3& box);
	};

	using TraceType			= TraceInfo::Type;

	bool					TraceRay(TraceInfo& trace) const;

	using Light				= PackedMap::Light;
	Light					lights[MAX_NUM_LIGHTS];
	u16						num_lights;

	void					ComputeLighting(bool shadows = true);
	void					UpdateLightmapTexture();

	void					Render();


private:
	bool					TraceRayStep(TraceInfo& trace, u16 node_index, float tmin, float tmax) const;
	void					SplitNode(u16 index, i16 bounds[2][3]);
	void					DoSplit(u16 node, const i16 bounds[2][3], u8 axis, i16 clip[2], i16& mid);
	void					LoadPatches(const PackedMap& packed, u8 pass);
	void					CreatePartition();

	void					InitLights();
	void					InitLightmap();

	static u16				MirrorPlaneIndex(u16 original);
} g_map;

////////////////////////////////////////////////////////////////

#include "map_patch.h"
#include "map_partition.h"
#include "map_lightmap.h"

// The first 6 brush planes correspond to the 6 sides of the bounding box.
// In order to maintain this order For mirrored brushes we need to swap
// the Y min/max planes (#2 & #3).
FORCEINLINE u16 Map::MirrorPlaneIndex(u16 brush_plane) {
	if (u16(brush_plane - 2) < 2)
		brush_plane ^= 2 ^ 3;
	return brush_plane;
}

FORCEINLINE void Map::InitLights() {
	using namespace Demo;

	if (EnableSunLight) {
		// FIXME: hardcoded
		auto& sun = lights[num_lights++];
		sun.color = 1.f;
		sun.position.z = 8192.f;
		sun.intensity = 32.f;
	}

	for (u16 light_index = 0; light_index < source->num_lights; ++light_index) {
		auto& light = lights[num_lights++];
		source->GetLight(light_index, light);
		if (light.position[symmetry_axis] < symmetry_level - 1) {
			auto& light2 = lights[num_lights++];
			MemCopy(&light2, &light, sizeof(light));
			light2.position[symmetry_axis] = 2.f * symmetry_level - light2.position[symmetry_axis];
			if (EnableSpotlights && light2.flags & Light::IsSpotlight)
				light2.spot[symmetry_axis] = -light2.spot[symmetry_axis];
		}
	}

	if (EnableAreaLights) {
		for (u32 plane_index = 0; plane_index < brushes.plane_count; ++plane_index) {
			auto material = brushes.GetPlaneMaterial(plane_index);
			auto emissive = Material::UnpackSurfaceLight(Material::Lights[material]);
			if (emissive.intensity == 0)
				continue;

			auto vtx_range = brushes.plane_vertex_range[plane_index];
			auto vtx_count = vtx_range.GetCount();
			if (vtx_count <= 3)
				continue;
	
			auto vtx_offset = vtx_range.GetOffset();
			vec3* pos = positions + vtx_offset;
			vec3 center = pos[0];
			for (u8 i = 1; i < vtx_count; ++i)
				center += pos[i];
			center /= vtx_count;

			assert(num_lights < MAX_NUM_LIGHTS);
			const vec4& plane = brushes.planes[plane_index];
			auto& light = lights[num_lights++];
		
			light.position = center;
			mad(light.position, plane.xyz, 0.125f);
			light.intensity = 70.f;
			light.color = emissive.color;
		}
	}
}

NOINLINE void Map::Load(const PackedMap& packed) {
	source = &packed;
	num_materials = packed.num_materials;

	assert(num_materials <= Demo::Material::Count);

	// TODO: some data initialization was skipped since we know we already have zeroes everywhere,
	// this should be fixed if support for loading different maps is ever added

	for (u16 pass = 0; pass < 2; ++pass) {
		auto* nonaxial_offset = packed.nonaxial_counts;
		auto* plane_data = packed.plane_data;
		auto* bounds_src = packed.brush_bounds;

		if (pass == 1) {
			u32 vertex_offset = 0;
			u32 index_offset = 0;
			for (u8 mat = 0; mat < num_materials; ++mat) {
				mat_vertex_offset[mat] = vertex_offset;
				vertex_offset += num_mat_verts[mat];
				num_mat_verts[mat] = 0;

				mat_index_offset[mat] = index_offset;
				index_offset += num_mat_indices[mat];
				num_mat_indices[mat] = 0;
			}
		}
		
		brushes.count = 0;
		brushes.plane_count = 0;

		u16 src_plane_index = 0;
		for (u16 brush_index = 0; brush_index < packed.num_brushes; ++brush_index) {
			u16 num_brush_planes = 0;

			auto dst_brush_index = brushes.count++;
			auto& brush_start = brushes.start[dst_brush_index];
			brush_start = brushes.plane_count;
			
			vec4* brush_planes = brushes.planes + brushes.plane_count;
			assert(brushes.plane_count + 6 <= MAX_NUM_PLANES);

			auto& brush_bounds = brushes.bounds[dst_brush_index];
			brush_bounds[0][0] = packed.world_bounds[0] + bounds_src[0];
			brush_bounds[0][1] = packed.world_bounds[1] + bounds_src[1];
			brush_bounds[0][2] = packed.world_bounds[2] + bounds_src[2];
			brush_bounds[1][0] = bounds_src[3] + brush_bounds[0][0];
			brush_bounds[1][1] = bounds_src[4] + brush_bounds[0][1];
			brush_bounds[1][2] = bounds_src[5] + brush_bounds[0][2];

			bool mirrored = brush_bounds[1][1] < symmetry_level + 1;

			bounds_src += 6;

			/* first 6 planes - one for each bounding box side */
			for (u16 i = 0; i < 6; ++i) {
				u16 axis = i >> 1;
				bool max_side = i & 1;
				u32 sign = i << 31;	// (i & 1) << 31
				float value = brush_bounds[max_side][axis];
				vec4& plane = brush_planes[num_brush_planes++];
				for (u16 j = 0; j < 3; ++j)
					plane.data[j] = 0;
				//plane.data[axis] = max_side ? 1.f : -1.f;
				*((u32*)&plane.data[axis]) = 0xbf80'0000u ^ sign;
				//plane.data[3] = max_side ? -value : value;
				*((u32*)&plane.data[3]) = *(u32*)&value ^ sign;
			}

			const i32
				OctBits			= 12,
				OctLevels		= 1 << OctBits,
				DistFractBits	= 4,
				DistScale		= 1 << DistFractBits;

			/* non-axial planes, if any */
			if (brush_index >= packed.num_brushes - packed.num_nonaxial_brushes) {
				auto num_extra_planes = *nonaxial_offset++;
				assert(brushes.plane_count + 6  + num_extra_planes <= MAX_NUM_PLANES);
				
				for (u32 i = 0; i < num_extra_planes; ++i) {
					vec4& plane = brush_planes[num_brush_planes++];
				
					u32 xy = plane_data[0];
					i32 w = plane_data[packed.num_unaligned_planes];
					++plane_data;
				
					vec2 oct;
					i32 x = xy & (OctLevels - 1);
					i32 y = xy >> 16;
					oct.x = max(-1.f, (x - i32(OctLevels >> 1)) * (1.f / float((OctLevels >> 1) - 1)));
					oct.y = max(-1.f, (y - i32(OctLevels >> 1)) * (1.f / float((OctLevels >> 1) - 1)));

					plane.xyz = oct_to_vec3(oct);
					plane.w = float(w) * (1.f / float(DistScale));
				}
			}

			brushes.plane_count += num_brush_planes;
			if (mirrored) {
				brushes.start[brushes.count] = brushes.plane_count;
				
				auto& dst_bounds = brushes.bounds[brushes.count];
				dst_bounds[0][0] = brush_bounds[0][0];
				dst_bounds[0][1] = 2 * symmetry_level - brush_bounds[1][1];
				dst_bounds[0][2] = brush_bounds[0][2];
				dst_bounds[1][0] = brush_bounds[1][0];
				dst_bounds[1][1] = 2 * symmetry_level - brush_bounds[0][1];
				dst_bounds[1][2] = brush_bounds[1][2];
				
				for (u16 i = 0; i < num_brush_planes; ++i) {
					auto& dst_plane = brush_planes[i + num_brush_planes];
					auto& src_plane = brush_planes[MirrorPlaneIndex(i)];
					dst_plane.x = src_plane.x;
					dst_plane.y = -src_plane.y;
					dst_plane.z = src_plane.z;
					dst_plane.w = src_plane.w + 2.f * src_plane.y * symmetry_level;
				}
				
				brushes.plane_count += num_brush_planes;
				
				++brushes.count;
			}

			const u16 MAX_NUM_EDGES = 48;
			BrushEdge brush_edges[MAX_NUM_EDGES];
		
			u16 num_brush_edges = EnumerateBrushEdges(brush_planes, num_brush_planes, brush_edges, MAX_NUM_EDGES);

			assert(num_brush_edges <= MAX_NUM_EDGES);
			assert(num_brush_planes <= 32);

			u32 edge_mask[MAX_NUM_EDGES];
			for (u16 i = 0; i < num_brush_edges; ++i) {
				auto& e = brush_edges[i];
				edge_mask[i] = (1 << e.first_plane) | (1 << e.second_plane);
			
				if (SnapVertices) {
					for (u16 j = 0; j < 3; ++j) {
						e.first_point [j] = floor(0.5f + e.first_point [j]);
						e.second_point[j] = floor(0.5f + e.second_point[j]);
					}
					if (length_squared(e.first_point - e.second_point) < 0.25f)
						edge_mask[i] = 0;
				}
			}

			/* iterate through all brush planes */
			for (u32 i = 0; i < num_brush_planes; ++i) {
				const u32 MAX_FACE_EDGES = 32;
				u8 face_edges[MAX_FACE_EDGES];
				u32 num_face_edges = 0;

				u32 face_mask = 1 << i;
				for (u32 j = 0; j < num_brush_edges; ++j) {
					if (edge_mask[j] & face_mask)
						face_edges[num_face_edges++] = j;
				}

				auto uv = packed.GetPlaneUV(src_plane_index);
				auto material = packed.plane_materials[src_plane_index];
				++src_plane_index;

				brushes.plane_mat_uv_axis[brush_start + i] = material;
				if (mirrored)
					brushes.plane_mat_uv_axis[brush_start + MirrorPlaneIndex(i) + num_brush_planes] = material;

				if (num_face_edges < 3)
					continue;
				
				u8 uv_axis = material & 3;
				material >>= 2;

				bool needs_uv = Demo::Material::Properties[material] & Demo::Material::NeedsUV;

				vec3 center = 0.f;
				for (u32 j = 0; j < num_face_edges; ++j) {
					auto& e = brush_edges[face_edges[j]];
					center += e.first_point;
					center += e.second_point;
				}
				center /= float(2 * num_face_edges);
				auto& ref_edge = brush_edges[face_edges[0]];
				auto& plane = brush_planes[i];
				
				vec3 x_axis = ref_edge.first_point;
				x_axis += ref_edge.second_point;
				x_axis *= 0.5f;
				x_axis -= center;
				
				vec3 y_axis = cross(x_axis, plane.xyz);

				float edge_angle[MAX_FACE_EDGES];
				for (u32 j = 0; j < num_face_edges; ++j) {
					auto edge_index = face_edges[j];
					auto& edge = brush_edges[edge_index];
					vec3 delta = (edge.first_point + edge.second_point) * 0.5f - center;
					edge_angle[edge_index] = Math::atan2(dot(delta, y_axis), dot(delta, x_axis));
				}

				SimpleSort(face_edges, num_face_edges, [&] (u8 a, u8 b) {
					return edge_angle[a] > edge_angle[b];
				});

				if (pass == 0) {
					num_mat_verts[material] += num_face_edges * (mirrored + 1);
					num_mat_indices[material] += (num_face_edges - 2) * 3 * (mirrored + 1);
					continue;
				}

				/* Set up UV mapping */

				vec2 texture_size{256.f, 256.f};
				u16 texture = Demo::MaterialTextures[material];
				if (texture < size(Demo::Texture::Descriptors)) {
					auto& descriptor = Demo::Texture::Descriptors[texture];
					texture_size.x = descriptor.width;
					texture_size.y = descriptor.height;
				}

				u8 s_axis = (uv_axis == 0);			// 1 0 0
				u8 t_axis = (uv_axis != 2) + 1;		// 2 2 1
				vec2 rot{sin(uv.angle), cos(uv.angle)};
				
				auto uv_map = [&] (const vec3& v) -> vec2 {
					vec2 st{v[s_axis], v[t_axis]};
					st = st.x * vec2{rot.y, rot.x} + st.y * vec2{-rot.x, rot.y};
					st /= uv.scale;
					st.x += uv.offset.x;
					st.y -= uv.offset.y;
					st /= texture_size;
					return st;
				};

				u32 first_vertex = num_mat_verts[material];
				for (u32 j = 0; j < num_face_edges; ++j) {
					auto& edge = brush_edges[face_edges[j]];
					u32 index = mat_vertex_offset[material] + num_mat_verts[material]++;
					positions[index] = (i == edge.first_plane) ? edge.first_point : edge.second_point;
					texcoords[index].xy = uv_map(positions[index]);
					normals[index] = plane.xyz;
				}
				
				brushes.plane_vertex_range[brush_start + i].Set(mat_vertex_offset[material] + first_vertex, num_face_edges);
				if (mirrored) {
					brushes
						.plane_vertex_range[brush_start + MirrorPlaneIndex(i) + num_brush_planes]
						.Set(mat_vertex_offset[material] + first_vertex + num_face_edges, num_face_edges);
				}

				/* Recompute the planes to match the (snapped) rendered geometry */
				if (RecomputePlanes) {
					vec3* v = positions + mat_vertex_offset[material] + first_vertex;

					auto& src_plane = brush_planes[i];
					safe_normalize(cross(v[1] - v[0], v[2] - v[1]), src_plane.xyz);
					assert(length_squared(src_plane.xyz) > 0.25f);
					src_plane.w = -dot(v[0], src_plane.xyz);

					if (mirrored) {
						auto& dst_plane = brush_planes[MirrorPlaneIndex(i) + num_brush_planes];
						dst_plane.x = src_plane.x;
						dst_plane.y = -src_plane.y;
						dst_plane.z = src_plane.z;
						dst_plane.w = src_plane.w + 2.f * src_plane.y * symmetry_level;
					}
				}

				u32 index_offset = mat_index_offset[material];
				for (u32 j = 2; j < num_face_edges; ++j) {
					indices[index_offset + num_mat_indices[material]++] = first_vertex;
					indices[index_offset + num_mat_indices[material]++] = first_vertex + j - 1;
					indices[index_offset + num_mat_indices[material]++] = first_vertex + j;
				}

				if (mirrored) {
					for (u16 j = 0; j < num_face_edges; ++j) {
						u32 dst_index = mat_vertex_offset[material] + j + num_mat_verts[material];
						u32 src_index = mat_vertex_offset[material] + j + first_vertex;
						vec3& dst_pos = positions[dst_index];
						vec3& src_pos = positions[src_index];
						dst_pos.x = src_pos.x;
						dst_pos.y = 2 * symmetry_level - src_pos.y;
						dst_pos.z = src_pos.z;
						if (needs_uv)
							texcoords[dst_index] = texcoords[src_index];
						else
							texcoords[dst_index].xy = uv_map(dst_pos);
						auto& dst_nor = normals[dst_index];
					}
					num_mat_verts[material] += num_face_edges;
					first_vertex += num_face_edges;
					for (u32 j = 2; j < num_face_edges; ++j) {
						indices[index_offset + num_mat_indices[material]++] = first_vertex;
						indices[index_offset + num_mat_indices[material]++] = first_vertex + j;
						indices[index_offset + num_mat_indices[material]++] = first_vertex + j - 1;
					}
				} // mirrored
			} // planes
		} // brushes

		assert(brushes.count < MAX_NUM_BRUSHES);
		// terminator
		brushes.start[brushes.count] = brushes.plane_count;

		assert(src_plane_index == packed.num_planes);

		LoadPatches(packed, pass);
	}

	u32 total_verts = 0;
	u32 total_indices = 0;

	for (u8 material = 0; material < num_materials; ++material) {
		vec3* pos = positions + mat_vertex_offset[material];
		vec3* nor = normals + mat_vertex_offset[material];
		u16* idx = indices + mat_index_offset[material];

		u32 num_verts = num_mat_verts[material];
		u32 num_indices = num_mat_indices[material];
		assert(num_verts <= 0x10000u);

		total_verts += num_verts;
		total_indices += num_indices;

		// TODO: zero normals out if loading multiple maps

		for (u32 i = 0; i < num_indices; i += 3, idx += 3) {
			for (u8 j = 0; j < 3; ++j) {
				u16 i0 = idx[j];
				u16 i1 = idx[(j + 1) % 3];
				u16 i2 = idx[(j + 2) % 3];
				nor[i0] += cross(pos[i1] - pos[i0], pos[i2] - pos[i1]);
			}
		}

		for (u32 i = 0; i < num_verts; ++i) {
			nor[i] /= length(nor[i]);
		}
	}

	assert(total_verts <= MAX_NUM_VERTS);
	assert(total_indices <= MAX_NUM_INDICES);

	InitLights();
	CreatePartition();
	InitLightmap();
	ComputeLighting(false);
}

////////////////////////////////////////////////////////////////

void Map::Render() {
	using namespace Demo;

	const bool ShowClipping = false;

	Gfx::Mesh mesh;
	memset(&mesh, 0, sizeof(mesh));

	// superfluous since we're using floating-point data,
	// but it seems to help crinkler and it does no harm...
	mesh.vertices[Attrib::Normal].normalized = true;

	auto num_materials = min<u8>(size(Material::Properties), this->num_materials);
	for (u8 material = 0; material < num_materials; ++material) {
		auto draw = Material::Properties[material] & Material::MaskVisibility;
		if (draw == Material::Invisible && !ShowClipping)
			continue;

		assert(num_mat_verts[material] < 0x10000u);
		assert(num_mat_indices[material] < 0x10000u);

		auto offset = mat_vertex_offset[material];
		mesh.vertices[Attrib::Position	].SetData(offset + positions);
		mesh.vertices[Attrib::TexCoord	].SetData(offset + texcoords);
		mesh.vertices[Attrib::Normal	].SetData(offset + normals);
		
		mesh.indices			= indices + mat_index_offset[material];
		mesh.num_vertices		= num_mat_verts[material];
		mesh.num_indices		= num_mat_indices[material];

		if (!mesh.num_vertices || !mesh.num_indices)
			continue;

		Gfx::SetShader(MaterialShaders[material]);
		
		Uniform::Time.w = material;
		Uniform::Texture0 = MaterialTextures[material];
		Uniform::Texture1 = Texture::Lightmap;
		
		if (r_lightmap.i)
			Uniform::Texture0 = Texture::Grey;

		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
	}

	if (0) {
		const u16 plane_index = 256;//1482;

		auto vtx_range = brushes.plane_vertex_range[plane_index];
		auto material = brushes.GetPlaneMaterial(plane_index);
		
		const u16 MaxIndices = 64 * 3;
		u16 indices[MaxIndices];
		u16 num_indices = 0;

		const u16 MaxVerts = 64;
		vec3 pos[MaxVerts];
		for (u16 i = 0; i < vtx_range.GetCount(); ++i)
			pos[i] = positions[vtx_range.GetOffset() + i] + normals[vtx_range.GetOffset() + i];

		//mesh.vertices[Attrib::Position	].SetData(vtx_range.GetOffset() + positions);
		mesh.vertices[Attrib::Position	].SetData(pos);
		mesh.vertices[Attrib::TexCoord	].SetData(vtx_range.GetOffset() + texcoords);
		mesh.vertices[Attrib::Normal	].SetData(vtx_range.GetOffset() + normals);

		for (u16 i = 2; i < vtx_range.GetCount(); ++i) {
			indices[num_indices++] = 0;
			indices[num_indices++] = i - 1;
			indices[num_indices++] = i;
		}
		
		mesh.indices			= indices;
		mesh.num_vertices		= vtx_range.GetCount();
		mesh.num_indices		= num_indices;

		Gfx::SetShader(Shader::Generic);
		
		Uniform::Time.w = 3;
		Uniform::Texture0 = Texture::White;
		Uniform::Texture1 = Texture::Lightmap;
		
		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
	}
}
