#pragma once

#include "entity.h"
#include "packed_map.h"

////////////////////////////////////////////////////////////////

namespace Demo {
	static constexpr u8 MaterialShaders[] = {
		#define PP_DEMO_MATERIAL_SHADER(path, shader, texture, contents, draw, light)		Demo::Shader::shader,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_SHADER)
		#undef PP_DEMO_MATERIAL_SHADER
	};

	static constexpr u8 MaterialTextures[] = {
		#define PP_DEMO_MATERIAL_TEXTURE(path, shader, texture, contents, draw, light)		Demo::Texture::texture,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_TEXTURE)
		#undef PP_DEMO_MATERIAL_TEXTURE
	};
}

////////////////////////////////////////////////////////////////

namespace Map {
	enum {
		MAX_NUM_VERTS		= 96 * 1024,
		MAX_NUM_MODEL_VERTS	= 16 * 1024,
		MAX_NUM_TRIS		= 128 * 1024,
		MAX_NUM_INDICES		= MAX_NUM_TRIS * 3,
		MAX_NUM_MATERIALS	= 256,
		
		MAX_NUM_BRUSHES		= 1024,
		MAX_NUM_PLANES		= 8192,

		MAX_NUM_PATCHES		= 256,
		
		MAX_NUM_NODES		= MAX_NUM_BRUSHES,
		MIN_NODE_BRUSHES	= 1,

		MAX_NUM_ENTITIES	= 256,
		MAX_NUM_LIGHTS		= 256,
	};

	enum {
		EnableSpotlights	= PackedMap::EnableSpotlights,
		EnableAreaLights	= false,
		SnapVertices		= true,
		RecomputePlanes		= false,
	};

	void					Load(const PackedMap& packed);

	i8						symmetry_axis;
	i16						symmetry_level;
	bool					UseSymmetry() { return u8(symmetry_axis) < 3; }

	/* Unpacked brush data */

	struct {
		using BoundsList				= Array<i16[2][3], MAX_NUM_BRUSHES>;

		u32								count;
		u32								plane_count;

		BoundsList 						bounds;
		Array<u16,  MAX_NUM_BRUSHES>	start;
		Array<vec4, MAX_NUM_PLANES>		planes;
		Array<u16,  MAX_NUM_PLANES>		plane_mat_uv_axis;	// material: 8, uv_axis: 2
		Array<vec2, MAX_NUM_PLANES>		plane_lmap_offset;
		Array<u8,   MAX_NUM_BRUSHES>	entity;

		u8								GetPlaneMaterial(u32 plane_index) const { return plane_mat_uv_axis[plane_index] >> 2; }
		u8								GetPlaneUVAxis(u32 plane_index) const { return plane_mat_uv_axis[plane_index] & 3; }
		static u8						GetSAxis(u8 uv_axis) { return (uv_axis == 0);     } // 1 0 0
		static u8						GetTAxis(u8 uv_axis) { return (uv_axis != 2) + 1; } // 2 2 1

		struct {
			u32							data;
			void						Set(u32 offset, u8 count)	{ data = (offset << 8) | count; }
			u32							GetOffset() const			{ return data >> 8; }
			u32							GetCount() const			{ return data & 255; }
		}								plane_vertex_range	[MAX_NUM_PLANES];
	} brushes;

	/* Unpacked patch data */

	struct {
		u16								count;
		Array<u8,  MAX_NUM_PATCHES>		source;
		Array<u16, MAX_NUM_PATCHES>		control_start;
		Array<u32, MAX_NUM_PATCHES>		vertex_start;
		Array<u16, MAX_NUM_PATCHES>		vertex_count;

		u16								GetSourceIndex(u16 patch_index) const { return source[patch_index] >> 1; }
		bool							IsMirrored(u16 patch_index) const { return source[patch_index] & 1; }
	} patches;

	/* Brush partition (BIH) */

	struct Partition {
		struct Node {
			i16				data[2];	// leaf: [begin, end];	node: [-first_child, axis]
			i16				clip[2];	// leaf: ignored;		node: [left_clip, right_clip]

			bool			IsLeaf() const				{ return data[0] >= 0; }
			u16				GetLeftChild() const		{ return -data[0]; }
			u16				GetRightChild() const		{ return -data[0] + 1; }
			u16				GetChild(bool second) const { return -data[0] + second; }
		};

		using NodeList		= Array<Node, MAX_NUM_NODES>;
		using BrushList		= Array<u16, MAX_NUM_BRUSHES>;

		NodeList			nodes;
		BrushList			brushes;
		u16					num_nodes;
	} partition;

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
		Type				type;
		float				z_offset;
		float				fraction;
		i16					plane;
		bool				start_solid;
		vec3				hit_point;
		vec3				hit_normal;
		u16*				touch_ents;
		u16					max_touch_ents;
		u16					num_touch_ents;

		void				SetBullet(const vec3& a, const vec3& b);
		void				SetLightmap(const vec3& a, const vec3& b);
		void				SetCollision(const vec3& a, const vec3& travel, const vec3& box);
	};

	using TraceType			= TraceInfo::Type;

	bool					TraceRay(TraceInfo& trace);

	/* Entity data */

	using EntityList				= Array<Demo::Entity, MAX_NUM_ENTITIES>;
	using EntityBrushOffsets		= Array<u16, MAX_NUM_ENTITIES + 1>;

	u8								num_entities;
	u8								num_brush_entities;
	EntityList						entities;
	EntityBrushOffsets				entity_brush_start;

	bool							IsWorldspawnBrush(u16 index) { return index < entity_brush_start[1]; }
	Demo::Entity*					PickTarget(i16 target);

	/* Renderable geometry */

	Array<vec3, MAX_NUM_VERTS>		positions;
	Array<vec4, MAX_NUM_VERTS>		texcoords;
	Array<vec3, MAX_NUM_VERTS>		normals;
	Array<u32,  MAX_NUM_VERTS>		colors;
	Array<u32,  MAX_NUM_INDICES>	indices;
	Array<u32,  MAX_NUM_MATERIALS>	num_mat_verts;
	Array<u32,  MAX_NUM_MATERIALS>	mat_vertex_offset;
	Array<u32,  MAX_NUM_MATERIALS>	num_mat_indices;
	Array<u32,  MAX_NUM_MATERIALS>	mat_index_offset;
	u32								num_total_vertices;
	u32								num_total_indices;
	
	u32								num_model_vertices;
	Array<u32, MAX_NUM_MODEL_VERTS>	model_vertex_indices;
	Array<u32, MAX_NUM_MODEL_VERTS>	model_vertex_sources;

	struct {
		u32							positions;
		u32							texcoords;
		u32							normals;
		u32							colors;
		u32							indices;
	}								gpu_addr;

	void							Render();

	const PackedMap*				source;

	struct {
		u32*						data;
#ifdef ENABLE_RADIOSITY
		u32*						bounce_data;
#endif // ENABLE_RADIOSITY
		vec3*						pos;
		vec3*						nor;
		RectPacker					packer;
	} lightmap;

	/* Light data */

	using Light						= PackedMap::Light;
	using LightList					= Array<Light, MAX_NUM_LIGHTS>;
	LightList						lights;
	u16								num_lights;

	enum class LightMode {
		Draft,
		Shadows,
		Bounce,
	};

	void							AllocLightmap();
	void							ComputeLighting(LightMode mode = LightMode::Shadows);
	void							UpdateLightmapTexture();

	/* Internal functions */
	namespace Details {
		bool						TraceRayStep(TraceInfo& trace, u16 node_index, float tmin, float tmax);
		void						SplitNode(u16 index, i16 bounds[2][3]);
		void						DoSplit(u16 node, const i16 bounds[2][3], u8 axis, i16 clip[2], i16& mid);
		void						LoadPatches(const PackedMap& packed, u8 pass);
		void						LoadModels(u8 pass);
		void						ComputeNormals();
		void						CreatePartition();

		void						InitEntities();
		void						InitLights();
		void						PackLightmap();
		void						DebugFillLightmap();
		void						SampleLighting(const vec3& pos, const vec3& nor, const vec3& x_axis, const vec3& y_axis, TraceInfo& trace, vec3& accum, LightMode mode = LightMode::Shadows);
		u32							ClampColor(const vec3& accum);
		void						GetUnoccludedPos(vec3& pos, const vec3& nor, const vec3& x_axis, const vec3& y_axis, TraceInfo& trace);

		static u16					MirrorPlaneIndex(u16 brush_plane, u8 axis);
	}
} // namespace Map

////////////////////////////////////////////////////////////////

#include "map_patch.h"
#include "map_partition.h"
#include "map_lightmap.h"

// The first 6 brush planes correspond to the 6 sides of the bounding box.
// In order to maintain this order for mirrored brushes we need to swap
// the min/max planes for the symmetry axis
FORCEINLINE u16 Map::Details::MirrorPlaneIndex(u16 brush_plane, u8 axis) {
	return brush_plane ^ ((brush_plane - (axis << 1)) < 2);
}

FORCEINLINE void Map::Details::InitLights() {
	using namespace Demo;

	// light 0 is the sun, always present
	assert(source->num_lights > 0);
	source->GetLight(0, lights[0]);
	num_lights = 1;

	// mirror all other lights (starting from 1)
	if (UseSymmetry()) {
		for (u16 light_index = 1; light_index < source->num_lights; ++light_index) {
			auto& light = lights[num_lights++];
			source->GetLight(light_index, light);
			if (light.position[symmetry_axis] < symmetry_level - 1) {
				auto& light2 = lights[num_lights++];
				MemCopy(&light2, &light);
				light2.position[symmetry_axis] = 2.f * symmetry_level - light2.position[symmetry_axis];
				if (EnableSpotlights && light2.flags & Light::IsSpotlight)
					light2.spot[symmetry_axis] = -light2.spot[symmetry_axis];
			}
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

////////////////////////////////////////////////////////////////

Demo::Entity* Map::PickTarget(i16 target) {
	const u16 MaxChoices = 32;
	u16 choices[MaxChoices];
	u16 num_choices = 0;

	if (target) {
		for (auto *entity = &entities[num_brush_entities], *end = &entities[num_entities]; entity != end; ++entity) {
			if (entity->targetname == target) {
				choices[num_choices++] = entity - &entities[0];
				if (num_choices == MaxChoices)
					break;
			}
		}
	}

	if (num_choices > 0)
		return &entities[choices[Random() % num_choices]];
	return nullptr;
}

FORCEINLINE void Map::Details::InitEntities() {
	num_brush_entities = source->num_brush_entities;
	num_entities = source->num_entities;

	u16 brush_offset = 0;
	for (u8 entity_index = 0; entity_index < num_brush_entities; ++entity_index) {
		entity_brush_start[entity_index] = brush_offset;
		brush_offset += source->entity_brushes[entity_index];
	}
	entity_brush_start[num_brush_entities] = brush_offset;

	const u16 NumRawFields = sizeof(Demo::Entity) / sizeof(i16);
	const i16* src_data = source->entity_data;
	for (u16 field = 0; field < NumRawFields; ++field, src_data += num_entities) {
		i16* dst_data = (i16*)&entities[0] + field;
		for (u16 entity_index = 0; entity_index < num_entities; ++entity_index, dst_data += NumRawFields) {
			*dst_data = src_data[entity_index];
		}
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::Details::LoadModels(u8 pass) {
	using namespace Demo;

	Map::num_model_vertices = 0;

	for (const Entity *entity = &entities[num_brush_entities], *entity_end = &entities[num_entities]; entity != entity_end; ++entity) {
		if (entity->type != Entity::Type::misc_model)
			continue;

		assert(u32(entity->model - 1) < Model::Count);

		const u16
			part_begin = Model::Storage::first_model_part[entity->model - 1],
			part_end   = Model::Storage::first_model_part[entity->model]
		;

		float angle = float(entity->angle) * Math::DEG2RAD;
		mat4 transform = MakeRotation({angle, 0.f, 0.f});
		for (u16 i = 0; i < 3; ++i)
			transform.GetPosition()[i] = entity->origin[i];

		for (u16 part_index = part_begin; part_index < part_end; ++part_index) {
			const Model::Part& part = Model::Storage::parts[part_index];
			u16 material = part.material;
			bool is_sprite = Demo::Material::Properties[material] & Demo::Material::Sprite;

			if (pass != 0) {
				u32 first_vertex   = Map::num_mat_verts[material];
				u32 dst_vtx_offset = Map::mat_vertex_offset[material] + first_vertex;
				u32 src_vtx_offset = part.ofs_verts;

				for (u32 i = 0; i < part.num_verts; ++i) {
					vec3& pos = Map::positions[dst_vtx_offset + i];
					vec3& nor = Map::normals  [dst_vtx_offset + i];
					vec4& uv  = Map::texcoords[dst_vtx_offset + i];

					// Transformed vertex position
					pos = transform * Model::Storage::vertices[src_vtx_offset + i];

					// Original UVs, if present
					uv.xy = Model::Storage::uvs[src_vtx_offset + i];

					// Since misc_models aren't lightmapped, we can store other data in the lightmap UVs.
					// In order to be able to reconstruct the local-space normal inside the shader,
					// we save the angle of the entity in first unused channel.
					uv.z = angle;
					uv.w = 0.f;

					if (!is_sprite) {
						// Keep a list of all misc_model vertices (for vertex lighting computation)
						Map::model_vertex_indices[Map::num_model_vertices] = dst_vtx_offset + i;
						Map::model_vertex_sources[Map::num_model_vertices] = src_vtx_offset + i;
						++Map::num_model_vertices;
					} else {
						nor = Model::Storage::normals[src_vtx_offset + i];
					}
				}

				u32 dst_idx_offset = Map::mat_index_offset[material] + Map::num_mat_indices[material];
				u32 src_idx_offset = part.ofs_idx;
				for (u32 i = 0; i < part.num_indices; ++i)
					Map::indices[dst_idx_offset + i] = Model::Storage::indices[src_idx_offset + i] + first_vertex;
			} else {
				Map::num_model_vertices += part.num_verts;
				assert(Map::num_model_vertices <= MAX_NUM_MODEL_VERTS);
			}

			Map::num_mat_verts[material] += part.num_verts;
			Map::num_mat_indices[material] += part.num_indices;
		}
	}
}

////////////////////////////////////////////////////////////////

NOINLINE void Map::Load(const PackedMap& packed) {
	source = &packed;
	symmetry_axis = packed.symmetry_axis;
	symmetry_level = packed.symmetry_level;

	MemSet(&num_mat_verts);
	MemSet(&num_mat_indices);

	Details::InitEntities();

	for (u16 pass = 0; pass < 2; ++pass) {
		auto* nonaxial_offset = packed.nonaxial_counts;
		auto* plane_data = packed.plane_data;
		auto* bounds_src = packed.brush_bounds;

		if (pass == 1) {
			num_total_vertices = 0;
			num_total_indices = 0;
			for (u8 mat = 0; mat < Demo::Material::Count; ++mat) {
				mat_vertex_offset[mat] = num_total_vertices;
				num_total_vertices += num_mat_verts[mat];
				num_mat_verts[mat] = 0;

				mat_index_offset[mat] = num_total_indices;
				num_total_indices += num_mat_indices[mat];
				num_mat_indices[mat] = 0;
			}
		}
		
		brushes.count = 0;
		brushes.plane_count = 0;

		u16 src_plane_index = 0;
		u16 entity_index = 0;
		for (u16 brush_index = 0; brush_index < packed.num_brushes; ++brush_index) {
			u16 num_brush_planes = 0;
			if (brush_index >= entity_brush_start[entity_index + 1])
				++entity_index;

			auto dst_brush_index = brushes.count++;
			auto& brush_start = brushes.start[dst_brush_index];
			brush_start = brushes.plane_count;
			brushes.entity[dst_brush_index] = entity_index;
			
			vec4* brush_planes = brushes.planes + brushes.plane_count;
			assert(brushes.plane_count + 6 <= MAX_NUM_PLANES);

			auto& brush_bounds = brushes.bounds[dst_brush_index];
			brush_bounds[0][0] = packed.world_bounds[0] + bounds_src[0];
			brush_bounds[0][1] = packed.world_bounds[1] + bounds_src[1];
			brush_bounds[0][2] = packed.world_bounds[2] + bounds_src[2];
			brush_bounds[1][0] = bounds_src[3] + brush_bounds[0][0];
			brush_bounds[1][1] = bounds_src[4] + brush_bounds[0][1];
			brush_bounds[1][2] = bounds_src[5] + brush_bounds[0][2];

			bool mirrored =
				UseSymmetry() && entity_index == 0 &&
				brush_bounds[1][symmetry_axis] < symmetry_level + 1 &&
				packed.brush_asymmetry[brush_index] == 0;

			bounds_src += 6;

			/* first 6 planes - one for each bounding box side */
			MemSet(brush_planes, 0, 6);
			for (u16 i = 0; i < 6; ++i) {
				u16 axis = i >> 1;
				bool max_side = i & 1;
				u32 sign = i << 31;	// (i & 1) << 31
				float value = brush_bounds[max_side][axis];
				vec4& plane = brush_planes[num_brush_planes++];
				//plane.data[axis] = max_side ? 1.f : -1.f;
				*((u32*)&plane.data[axis]) = 0xbf80'0000u ^ sign;
				//plane.data[3] = max_side ? -value : value;
				*((u32*)&plane.data[3]) = *(u32*)&value ^ sign;
			}

			/* non-axial planes, if any */
			{
				auto num_extra_planes = *nonaxial_offset++;
				assert(brushes.plane_count + 6  + num_extra_planes <= MAX_NUM_PLANES);

				for (u32 i = 0; i < num_extra_planes; ++i)
					packed.GetPlane(plane_data, brush_bounds, brush_planes[num_brush_planes++]);
			}

			brushes.plane_count += num_brush_planes;
			if (mirrored) {
				brushes.start[brushes.count] = brushes.plane_count;
				
				auto& dst_bounds = brushes.bounds[brushes.count];
				MemCopy(&dst_bounds, &brush_bounds);
				dst_bounds[0][symmetry_axis] = 2 * symmetry_level - brush_bounds[1][symmetry_axis];
				dst_bounds[1][symmetry_axis] = 2 * symmetry_level - brush_bounds[0][symmetry_axis];
				
				for (u16 i = 0; i < num_brush_planes; ++i) {
					auto& dst_plane = brush_planes[i + num_brush_planes];
					auto& src_plane = brush_planes[Details::MirrorPlaneIndex(i, symmetry_axis)];
					dst_plane.xyz = src_plane.xyz;
					dst_plane[symmetry_axis] = -dst_plane[symmetry_axis];
					dst_plane.w = src_plane.w + 2.f * src_plane[symmetry_axis] * symmetry_level;
				}
				
				brushes.plane_count += num_brush_planes;
				
				++brushes.count;
			}

			const u16 MAX_NUM_EDGES = 48;
			BrushEdge brush_edges[MAX_NUM_EDGES];
		
			u16 num_brush_edges = EnumerateBrushEdges(brush_planes, num_brush_planes, brush_edges, MAX_NUM_EDGES, 1.f/16.f);

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
				auto material = packed.plane_materials[src_plane_index]; // bits 0..1 = dominant axis; bits 2+ = material
				// optimization: for the first 6 planes of a brush the dominant axis is not filled in by the map compiler
				if (i < 6)
					material |= i >> 1;
				++src_plane_index;

				brushes.plane_mat_uv_axis[brush_start + i] = material;
				if (mirrored)
					brushes.plane_mat_uv_axis[brush_start + Details::MirrorPlaneIndex(i, symmetry_axis) + num_brush_planes] = material;

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
					vec3 delta;
					delta.x = (edge.first_point.x + edge.second_point.x) * 0.5f - center.x;
					delta.y = (edge.first_point.y + edge.second_point.y) * 0.5f - center.y;
					delta.z = (edge.first_point.z + edge.second_point.z) * 0.5f - center.z;
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

				u8 s_axis = brushes.GetSAxis(uv_axis);
				u8 t_axis = brushes.GetTAxis(uv_axis);
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
						.plane_vertex_range[brush_start + Details::MirrorPlaneIndex(i, symmetry_axis) + num_brush_planes]
						.Set(mat_vertex_offset[material] + first_vertex + num_face_edges, num_face_edges);
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
						dst_pos = src_pos;
						dst_pos[symmetry_axis] = 2 * symmetry_level - src_pos[symmetry_axis];
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
#ifdef DEV
		assert(plane_data == packed.plane_data + packed.num_plane_entries);
#endif

		Details::LoadPatches(packed, pass);
		Details::LoadModels(pass);
	}

	Details::ComputeNormals();

	assert(Map::num_total_vertices <= MAX_NUM_VERTS);
	assert(Map::num_total_indices <= MAX_NUM_INDICES);

	/* Recompute the planes to match the (snapped) rendered geometry */
	if (RecomputePlanes) {
		for (u32 plane_index = 0; plane_index < brushes.plane_count; ++plane_index) {
			vec4& plane = brushes.planes[plane_index];
			auto vtx_range = brushes.plane_vertex_range[plane_index];

			for (const vec3* nor = normals + vtx_range.GetOffset(), *end = nor + vtx_range.GetCount(); nor != end; ++nor) {
				if (length_squared(*nor) != 0.f) {
					plane.xyz = *nor;
					plane.w = -dot(positions[vtx_range.GetOffset()], *nor);
					break;
				}
			}
		}
	}

	/* HACK: pack misc_model original positions inside the normals (for local-space texturing) */
	for (u32 i = 0; i < num_model_vertices; ++i) {
		u32 index = model_vertex_indices[i];
		u32 source = model_vertex_sources[i];
		using namespace Demo;
		Model::PackNormalOffset(Map::normals[index], Model::Storage::vertices[source]);
	}

	Details::InitLights();
	Details::CreatePartition();
	Details::PackLightmap();
	ComputeLighting(LightMode::Draft);

	Gfx::ResetArena(Gfx::Arena::Level);
	gpu_addr.positions = Gfx::UploadGeometry(&positions[0], num_total_vertices, Gfx::Arena::Level);
	gpu_addr.texcoords = Gfx::UploadGeometry(&texcoords[0], num_total_vertices, Gfx::Arena::Level);
	gpu_addr.normals   = Gfx::UploadGeometry(&normals[0],   num_total_vertices, Gfx::Arena::Level);
	gpu_addr.colors    = Gfx::UploadGeometry(&colors[0],    num_total_vertices, Gfx::Arena::Level);
	gpu_addr.indices   = Gfx::UploadGeometry(&indices[0],   num_total_indices,  Gfx::Arena::Level);
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Map::Details::ComputeNormals() {
	for (u8 material = 0; material < Demo::Material::Count; ++material) {
		if (Demo::Material::Properties[material] & Demo::Material::Sprite)
			continue;

		vec3* pos = positions + mat_vertex_offset[material];
		vec3* nor = normals + mat_vertex_offset[material];
		u32* idx = indices + mat_index_offset[material];

		Demo::ComputeNormals(pos, idx, num_mat_verts[material], num_mat_indices[material], nor);
	}
}

////////////////////////////////////////////////////////////////

void Map::Render() {
	using namespace Demo;

	const bool ShowClipping = false;

	Gfx::Mesh mesh;
	memset(&mesh, 0, sizeof(mesh));

	mesh.vertices[Attrib::Color].normalized = true;

	auto num_materials = Demo::Material::Count;
	for (u8 material = 0; material < num_materials; ++material) {
		auto visibility = Material::Properties[material] & Material::MaskVisibility;

#ifndef DRAW_CLIPPING
		if (visibility == Material::Invisible)
			continue;
#endif

		//if (!(Material::Properties[material] & Material::Sprite))
		//	continue;

		auto offset = mat_vertex_offset[material];
		mesh.vertices[Attrib::Position].SetData<vec3>(gpu_addr.positions, offset);
		mesh.vertices[Attrib::TexCoord].SetData<vec4>(gpu_addr.texcoords, offset);
		mesh.vertices[Attrib::Normal  ].SetData<vec3>(gpu_addr.normals,   offset);
		mesh.vertices[Attrib::Color   ].SetData<u32 >(gpu_addr.colors,    offset);

		mesh.index_addr   = gpu_addr.indices + mat_index_offset[material] * sizeof(u32);
		mesh.num_vertices = num_mat_verts[material];
		mesh.num_indices  = num_mat_indices[material];

		if (!mesh.num_vertices || !mesh.num_indices)
			continue;

		Gfx::SetShader(MaterialShaders[material]);

		Uniform::Time.w = material;
		Uniform::Texture0 = MaterialTextures[material];
		Uniform::Texture1 = Texture::Lightmap;

		if (r_lightmap.integer)
			Uniform::Texture0 = Texture::Grey;
		if (r_fullbright.integer)
			Uniform::Texture1 = Texture::Grey;

		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
	}

#if 0
	if (0) {
		const u16 plane_index = 256;//1482;

		auto vtx_range = brushes.plane_vertex_range[plane_index];
		auto material = brushes.GetPlaneMaterial(plane_index);
		
		const u16 MaxIndices = 64 * 3;
		u32 indices[MaxIndices];
		u32 num_indices = 0;

		const u16 MaxVerts = 64;
		vec3 pos[MaxVerts];
		for (u16 i = 0; i < vtx_range.GetCount(); ++i)
			pos[i] = positions[vtx_range.GetOffset() + i] + normals[vtx_range.GetOffset() + i];

		//mesh.vertices[Attrib::Position	].SetData(vtx_range.GetOffset() + positions);
		mesh.vertices[Attrib::Position	].SetData(pos);
		mesh.vertices[Attrib::TexCoord	].SetData(vtx_range.GetOffset() + texcoords);
		mesh.vertices[Attrib::Normal	].SetData(vtx_range.GetOffset() + normals);

		for (u32 i = 2; i < vtx_range.GetCount(); ++i) {
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
#endif
}
