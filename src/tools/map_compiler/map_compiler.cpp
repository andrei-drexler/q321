#include "../common.h"
#include "../../demo/resource_def.h"
#include "../../demo/material.h"
#include "../../demo/entity.h"
#include "q3map.h"
#include "../print_array.h"
#include "export_obj.h"
#include "export_map.h"
#include "../rect_packer.h"

using namespace Q3;

#define INDENT "   "

////////////////////////////////////////////////////////////////

struct Options {
	enum class UVSort {
		None,
		ByMapOrder,
		ByUsage,
	};
	
	struct Snap {
		float	uv_offset			= 1.f;
		float	uv_angle			= 1.f;
		int		uv_scale_bits		= 16;
		int		patch_uv_bits		= 12;
		float	patch_vertex		= 1.f;
	};

	const char* out_path			= "../../demo/cooked/cooked_maps.h";

	UVSort		sort_uv				= UVSort::ByMapOrder;
	Snap		snap;
	bool		reorder_materials	= false;
	bool		reuse_materials		= true;
	bool		trim_unwanted_uvs	= true;
	bool		wrap_uvs			= false; // increases final size by a bit
	bool		sort_lights			= true;
	bool		sort_patches		= false;
	bool		use_spotlights		= true;
	bool		use_area_lights		= true;

	bool		verbose				= false;

};

////////////////////////////////////////////////////////////////

template <typename T>
void Reorder(std::vector<T>& items, const size_t* order) {
	std::vector<T> sorted_items;
	sorted_items.reserve(items.size());
	for (size_t i = 0; i < items.size(); ++i) {
		sorted_items.emplace_back(std::move(items[order[i]]));
	}
	items.swap(sorted_items);
}

////////////////////////////////////////////////////////////////

constexpr u32 EncodeSignMagnitude(i32 i) {
	u32 s = u32(i) >> 31;
	return (((u32(i) ^ -s) + s) << 1) | s;
}

constexpr i32 DecodeSignMagnitude(u32 u) {
	bool negative = u & 1;
	u >>= 1;
	return negative ? -u : u;
}

////////////////////////////////////////////////////////////////

struct ShaderProperties {
	Content::Flags		content_flags;
	Surface::Flags		surface_flags;
	int16_t				map_material = -1;
	int16_t				map_material_base = -1;
	int16_t				width = 0;
	int16_t				height = 0;

	// ugh
	constexpr ShaderProperties(uint32_t contents = 0, uint32_t surface = 0, int16_t map_material = -1, int16_t map_material_base = -1) :
		content_flags		(static_cast<Content::Flags>(contents)),
		surface_flags		(static_cast<Surface::Flags>(surface)),
		map_material		(map_material),
		map_material_base	(map_material_base)
	{ }
};

struct PredefinedShader {
	string_view	name;
	ShaderProperties	props;
};

static constexpr PredefinedShader predefined_shaders[] = {
	{ "common/caulk",				{ Content::SOLID,												Surface::NODRAW }},
	{ "common/weapclip",			{ Content::SOLID,												Surface::NODRAW }},
	{ "common/clip",				{ Content::SOLID | Content::DETAIL,								Surface::NODRAW }},
	{ "common/nodraw",				{ Content::DETAIL | Content::TRANSLUCENT | Content::SOLID,		Surface::NODRAW }},
	{ "common/nodrawnonsolid",		{ Content::DETAIL | Content::TRANSLUCENT,						Surface::NODRAW | Surface::NONSOLID }},
	{ "common/nodrop",				{ Content::DETAIL | Content::NODROP,							Surface::NODRAW }},
	{ "common/trigger",				{ Content::TRIGGER,												Surface::NODRAW }},
	{ "common/origin",				{ Content::ORIGIN,												Surface::NODRAW }},
	{ "sfx/beam",					{ Content::DETAIL,												Surface::NODRAW }},
	{ "skies/blacksky",				{ Content::SOLID | Content::STRUCTURAL,							Surface::SKY }},
};

static const char* const DemoMaterialNames[] = {
	#define PP_DEMO_MATERIAL_DEF(name, ...)		name,
	DEMO_MATERIALS(PP_DEMO_MATERIAL_DEF)
	#undef PP_DEMO_MATERIAL_DEF
};

enum class DemoTexture {
	#define PP_DEMO_TEXTURE_ID(name, ...)		name,
	DEMO_TEXTURES(PP_DEMO_TEXTURE_ID)
	#undef PP_DEMO_TEXTURE_ID

	Count,
};

enum class DemoShader {
	#define PP_DEMO_SHADER_ID(name, ...)		name,
	DEMO_SHADERS(PP_DEMO_SHADER_ID)
	#undef PP_DEMO_SHADER_ID

	Count,
};

static DemoTexture const DemoMaterialTextures[] = {
	#define PP_DEMO_MATERIAL_TEXTURE(name, shader, texture, contents, draw, light)		DemoTexture::texture,
	DEMO_MATERIALS(PP_DEMO_MATERIAL_TEXTURE)
	#undef PP_DEMO_MATERIAL_TEXTURE
};

static DemoShader const DemoMaterialShaders[] = {
	#define PP_DEMO_MATERIAL_SHADER(name, shader, texture, contents, draw, light)		DemoShader::shader,
	DEMO_MATERIALS(PP_DEMO_MATERIAL_SHADER)
	#undef PP_DEMO_MATERIAL_SHADER
};

static constexpr struct TextureSize {
	int width, height;
} DemoTextureSizes[] = {
	#define PP_DEMO_TEXTURE_SIZE(name, procgen, width, height, format, flags)			{ width, height },
	DEMO_TEXTURES(PP_DEMO_TEXTURE_SIZE)
	#undef PP_DEMO_TEXTURE_SIZE
};

std::vector<ShaderProperties> GetShaderProperties(Map& map, const Options& options) {
	std::vector<ShaderProperties> prop_list(map.materials.size());
	
	for (size_t mat_index = 0; mat_index < map.materials.size(); ++mat_index) {
		auto& material = map.materials[mat_index];
		auto& props = prop_list[mat_index];

		for (auto& predefined : predefined_shaders) {
			if (material.name == predefined.name) {
				props = predefined.props;
				break;
			}
		}

		for (u16 i = 0; i < std::size(DemoMaterialNames); ++i) {
			if (0 == strcmp(DemoMaterialNames[i], material.name.c_str())) {
				props.map_material_base = i;

				if (options.reuse_materials) {
					// try to find an earlier material with the exact same properties
					for (u16 j = 0; j < i; ++j) {
						if (DemoMaterialShaders[i] == DemoMaterialShaders[j] &&
							DemoMaterialTextures[i] == DemoMaterialTextures[j] &&
							Demo::Material::Properties[i] == Demo::Material::Properties[j]) {
							i = j;
							break;
						}
					}
				}

				auto texture_id = DemoMaterialTextures[i];
				assert(texture_id < DemoTexture::Count);
				auto texture_size = DemoTextureSizes[(int)texture_id];

				props.map_material = i;
				props.width = texture_size.width;
				props.height = texture_size.height;
				break;
			}
		}
	}

	return prop_list;
}

////////////////////////////////////////////////////////////////

struct Symmetry {
	i16 axis = -1;
	i16 level = 0;

	constexpr explicit operator bool() const { return u16(axis) < 3; }
};

void ExploitSymmetry(Map& map, const Options& options, Symmetry& symmetry) {
	auto& world = map.World();
	auto text = world.GetProperty("_symmetry"sv);
	if (!ParseValue(text, symmetry.axis) || symmetry.axis >= 3 || !ParseValue(text, symmetry.level)) {
		symmetry = {};
		return;
	}

	printf("Exploiting symmetry (%c = %d)\n", 'X' + symmetry.axis, symmetry.level);

	auto trim = [&](auto& list) {
		size_t before = list.size();
		list.erase(std::remove_if(list.begin(), list.end(), [&](auto& item) {
			return item.bounds.mins[symmetry.axis] > symmetry.level - 1.f;
		}), list.end());
		return before - list.size();
	};

	auto num_erased_brushes = trim(world.brushes);
	auto num_erased_patches = trim(world.patches);
	printf(INDENT "removed %zd/%zd brushes and %zd/%zd patches\n",
		num_erased_brushes, world.brushes.size() + num_erased_brushes,
		num_erased_patches, world.patches.size() + num_erased_patches
	);
}

////////////////////////////////////////////////////////////////

void RemoveSkyNodropBrushes(Map& map, const ShaderProperties* props) {
	printf("Removing sky/nodrop brushes\n");

	size_t sky_count = 0, nodrop_count = 0;

	for (auto& ent : map.entities) {
		if (ent.brushes.empty())
			continue;
		
		for (auto i = ent.brushes.begin(), endi=ent.brushes.end(); i!=endi; /**/) {
			bool is_sky = false;
			bool is_nodrop = false;
			for (auto& plane : i->planes) {
				auto& plane_props = props[plane.material];
				is_sky |= (plane_props.surface_flags & Surface::SKY) != 0;
				is_nodrop |= (plane_props.surface_flags & Surface::NODRAW) != 0 && (plane_props.content_flags & Content::NODROP) != 0;
			}
			sky_count += is_sky;
			nodrop_count += is_nodrop;
			if (is_sky || is_nodrop) {
				i = ent.brushes.erase(i);
				endi = ent.brushes.end();
			} else {
				++i;
			}
		}
	}

	printf(INDENT "%zd sky/%zd nodrop brushes removed\n", sky_count, nodrop_count);
}

////////////////////////////////////////////////////////////////

const size_t ProgressBarLength = 64;
size_t ProgressDots(size_t current, size_t total) {
	if (!total)
		return ProgressBarLength;
	return (current * ProgressBarLength + (total >> 1)) / total;
}

void PrintProgress(size_t current, size_t total) {
	size_t old_len = current > 0 ? ProgressDots(current - 1, total) : 0;
	size_t len = ProgressDots(current, total);

	char buf[ProgressBarLength + 1];
	for (size_t i = old_len; i < len; ++i)
		buf[i - old_len] = '.';
	buf[len - old_len] = 0;
	printf("%s", buf);
}

bool AppendEntityGeometryToWorld(Map& map, size_t index) {
	auto& ent = map.entities[index];
	if (ent.brushes.empty() && ent.patches.empty())
		return false;

	auto& world = map.entities[0];
	world.brushes.insert(world.brushes.end(), ent.brushes.begin(), ent.brushes.end());
	world.patches.insert(world.patches.end(), ent.patches.begin(), ent.patches.end());
	world.bounds.add(ent.bounds);

	return true;
}

void MergeFuncGroups(Map& map) {
	if (map.entities.size() < 2)
		return;

	printf("Merging func_groups\n");

	auto& world = map.entities[0];
	for (auto i = map.entities.begin() + 1; i != map.entities.end(); /**/) {
		auto& ent = *i;
		//PrintProgress(i - map.entities.begin(), map.entities.size());
		//_sleep(100);
		if (ent.props["classname"] != "func_group"sv) {
			++i;
			continue;
		}
		AppendEntityGeometryToWorld(map, i - map.entities.begin());
		i = map.entities.erase(i);
	}
}

void RemoveUnknownEntities(Map& map, const Options& options, std::vector<Map::Entity>& lights) {
	printf("Removing unknown entities\n");

	std::unordered_map<string_view, Demo::Entity::Type> classname_to_type;
	#define PP_MAP_CLASSNAME_TO_TYPE(name, ...) classname_to_type[#name] = Demo::Entity::Type::name;
	DEMO_ENTITY_TYPES(PP_MAP_CLASSNAME_TO_TYPE)
	#undef PP_MAP_CLASSNAME_TO_TYPE

	auto last_known = std::stable_partition(map.entities.begin() + 1, map.entities.end(), [&] (Map::Entity& ent) {
		auto classname = ent.GetProperty("classname"sv);
		return classname_to_type.find(classname) != classname_to_type.end();
	});
	
	DebugPrint("Removed entities:\n");
	DebugPrint("-----------------\n");
	for (auto i = last_known; i != map.entities.end(); ++i) {
		auto& ent = *i;
		auto index = i - map.entities.begin();
		auto classname = ent.GetProperty("classname"sv);
		if (classname == "light"sv) {
			lights.push_back(std::move(ent));
			continue;
		}
		DebugPrint("%.*s\n", int(classname.size()), classname.data());
		if (options.verbose) {
			printf(INDENT "%.*s\n", int(classname.size()), classname.data());
		}
	}

	printf(INDENT "%zd entities removed\n", map.entities.end() - last_known);

	map.entities.erase(last_known, map.entities.end());
}

void SortEntities(Map& map, const Options& options) {
	printf("Sorting entities\n");

	std::unordered_map<std::string_view, size_t> type_count;
	std::unordered_map<std::string_view, bool> type_has_brushes;
	std::unordered_map<std::string_view, size_t> prop_count;
	std::unordered_map<std::string_view, std::unordered_set<std::string_view>> type_props;
	type_count.reserve(map.entities.size());
	type_has_brushes.reserve(map.entities.size());
	prop_count.reserve(map.entities.size() * 4);
	for (auto& ent : map.entities) {
		auto ent_index = &ent - map.entities.data();
		auto type = ent.GetProperty("classname"sv);
		++type_count[type];
		type_has_brushes[type] |= !ent.brushes.empty();
		for (auto& p : ent.props) {
			if (p.first == "classname"sv)
				continue;
			type_props[type].insert(p.first);
			++prop_count[p.first];
		}
	}

	std::vector<std::string_view> sorted_types;
	sorted_types.reserve(type_count.size());
	for (auto& type : type_count) {
		sorted_types.push_back(type.first);
	}
	std::sort(sorted_types.begin(), sorted_types.end(), [&] (string_view a, string_view b) {
		bool brushes_a = type_has_brushes[a];
		bool brushes_b = type_has_brushes[b];
		if (brushes_a != brushes_b)
			return brushes_a > brushes_b;
		return type_count[a] > type_count[b];
	});

	std::vector<std::string_view> sorted_properties;
	sorted_properties.reserve(prop_count.size());
	for (auto& prop : prop_count) {
		sorted_properties.push_back(prop.first);
	}
	std::sort(sorted_properties.begin(), sorted_properties.end(), [&] (string_view a, string_view b) {
		return prop_count[a] > prop_count[b];
	});

	DebugPrint("Entity properties:\n");
	DebugPrint("-----------------\n");
	for (auto& prop : sorted_properties) {
		DebugPrint("%.*s\n", int(prop.size()), prop.data());
	}

	DebugPrint("Entities:\n");
	DebugPrint("-----------------\n");
	printf(INDENT "%3d remaining entities:\n", map.entities.size());
	for (auto& type : sorted_types) {
		printf(INDENT "%3zd x %.*s\n", type_count[type], int(type.size()), type.data());
		DebugPrint("%.*s\n", int(type.size()), type.data());
		for (auto& prop : type_props[type])
			printf(INDENT "      %.*s\n", int(prop.size()), prop.data());
	}

	std::vector<size_t> entity_order(map.entities.size());
	for (size_t i = 0; i < entity_order.size(); ++i)
		entity_order[i] = i;

	std::sort(entity_order.begin() + 1, entity_order.end(), [&] (size_t a, size_t b) {
		i32 delta;
		auto& ent_a = map.entities[a];
		auto& ent_b = map.entities[b];

		/* brush entities before point entities */
		if ((delta = (ent_b.brushes.size() - ent_a.brushes.size())) != 0)
			return delta < 0;

		/* sort by frequency */
		auto class_a = ent_a.GetProperty("classname"sv);
		auto class_b = ent_b.GetProperty("classname"sv);
		if ((delta = (type_count[class_b] - type_count[class_a])) != 0)
			return delta < 0;
		
		return a < b;
	});
	Reorder(map.entities, entity_order.data());
}

void MergeEntityBrushes(Map& map, const Options& options, std::vector<size_t>& brush_count) {
	brush_count.resize(map.entities.size());
	for (size_t i = 0; i < map.entities.size(); ++i) {
		brush_count[i] = map.entities[i].brushes.size();
	}

	auto& world = map.entities[0];
	for (size_t i = 1; i < map.entities.size(); ++i) {
		if (!AppendEntityGeometryToWorld(map, i))
			continue;
		auto& ent = map.entities[i];
		if (options.verbose) {
			auto classname = ent.props["classname"];
			printf(INDENT "Merging %zd brushes from entity %zd (%.*s)\n", ent.brushes.size(), i, int(classname.size()), classname.data());
		}
		ent.brushes.clear();
		ent.patches.clear();
	}
}

void RebuildEntityLinks(Map& map, const Options& options) {
	printf("Reindexing entity links\n");

	std::unordered_map<std::string, size_t> targetname_to_index;
	targetname_to_index.reserve(map.entities.size());
	
	for (size_t i = 1, next_index = 1; i < map.entities.size(); ++i) {
		auto& ent = map.entities[i];
		auto targetname = ent.GetProperty("targetname"sv);
		if (targetname.empty())
			continue;
		auto& index = targetname_to_index[std::string{targetname}];
		if (!index)
			index = next_index++;
		char buffer[32];
		sprintf(buffer, "%zd", index);
		ent.props["targetname"] = buffer;
	}

	for (size_t i = 1, next_index = 1; i < map.entities.size(); ++i) {
		auto& ent = map.entities[i];
		auto target = ent.GetProperty("target"sv);
		if (target.empty())
			continue;
		auto iter = targetname_to_index.find(std::string{target});
		if (iter == targetname_to_index.end()) {
			printf(INDENT "Warning: no entity matching target '%.*s'\n", int(target.size()), target.data());
			ent.props.erase("target");
			continue;
		}
		char buffer[32];
		sprintf(buffer, "%zd", iter->second);
		ent.props["target"] = buffer;
	}
}

////////////////////////////////////////////////////////////////

void InsertMissingAxialPlanes(Map& map) {
	auto& world = map.World();

	printf("Inserting axial planes\n");

	for (auto& ent : map.entities) {
		for (auto& brush : ent.brushes) {
			if (brush.axial)
				continue;

			u32 found = 0;
			for (auto& plane : brush.planes)
				found |= 1 << plane.type;

			size_t num_original_planes = brush.planes.size();

			for (u32 i = 0; i < Map::Plane::Unaligned; ++i) {
				if (found & (1 << i))
					continue;

				auto type = Map::Plane::Type(i);
				auto axis = Map::Plane::GetAxis(type);
				bool max_side = Map::Plane::IsMaxSide(type);

				vec3 normal = vec3{0.f};
				normal[axis] = max_side ? 1.f : -1.f;
				
				// The bounding boxes will get snapped to 1 unit when written out.
				// To avoid having the non-axial faces clipped by the newly-added
				// axial planes, we enlarge the bounding boxes slightly (up to the
				// next unit).
				if (max_side)
					brush.bounds.maxs[axis] = ceil(brush.bounds.maxs[axis]);
				else
					brush.bounds.mins[axis] = floor(brush.bounds.mins[axis]);
				float plane_dist = max_side ? -brush.bounds.maxs[axis] : brush.bounds.mins[axis];
				
				int best_index = -1;
				float best_alignment = -FLT_MAX;
				for (size_t j = 0; j < num_original_planes; ++j) {
					auto& test_plane = brush.planes[j];
					float alignment = dot(test_plane.xyz, normal);
					if (best_index == -1 || alignment > best_alignment) {
						best_alignment = alignment;
						best_index = j;
					}
				}
				
				auto& source_plane	= brush.planes[best_index];
				auto& new_plane		= brush.planes.emplace_back(source_plane);
				new_plane.xyz		= normal;
				new_plane.w			= plane_dist;
				new_plane.type		= type;
				new_plane.synthetic	= true;
			}
		}
	}
}

////////////////////////////////////////////////////////////////

void SortMaterialsByUsage(Map& map, const Options& options) {
	printf("Sorting materials by usage\n");

	std::vector<size_t> usage(map.materials.size());
	for (auto& ent : map.entities) {
		for (auto& brush : ent.brushes)
			for (auto& plane : brush.planes)
				++usage[plane.material];
		for (auto& patch : ent.patches)
			++usage[patch.material];
	}

	std::vector<size_t> order(map.materials.size());
	for (size_t i = 0; i < order.size(); ++i)
		order[i] = i;

	std::sort(order.begin(), order.end(), [&] (size_t a, size_t b) {
		return usage[a] > usage[b];
	});

	std::vector<Map::Material>		sorted_materials(map.materials.size());
	std::vector<size_t>				remap(map.materials.size());
	for (size_t i = 0; i < order.size(); ++i) {
		remap[order[i]] = i;
		sorted_materials[i] = map.materials[order[i]];
		if (options.verbose)
			printf("%zd. %s [%zd]\n", i + 1, sorted_materials[i].name.c_str(), usage[order[i]]);
	}
	std::swap(map.materials, sorted_materials);

	for (auto& ent : map.entities) {
		for (auto& brush : ent.brushes)
			for (auto& plane : brush.planes)
				plane.material = remap[plane.material];
		for (auto& patch : ent.patches)
			patch.material = remap[patch.material];
	}
}

////////////////////////////////////////////////////////////////

void SortBrushesAndPlanes(Map& map) {
	printf("Sorting brushes/planes\n");

	std::vector<u16> dominant_material;
	dominant_material.reserve(1024);
	
	std::vector<u32> zorder_center;
	zorder_center.reserve(1024);

	std::vector<u16> material_usage(map.materials.size());

	for (auto& ent : map.entities) {
		dominant_material.resize(ent.brushes.size());
		zorder_center.resize(ent.brushes.size());

		auto ref_point = ent.bounds.mins;
		auto scale = 1023.f / ent.bounds.size();

		for (size_t i = 0; i < ent.brushes.size(); ++i) {
			auto& brush = ent.brushes[i];
			
			auto center = (brush.bounds.center() - ref_point) * scale;
			auto cx = std::clamp(i32(center.x), 0, 1023);
			auto cy = std::clamp(i32(center.y), 0, 1023);
			auto cz = std::clamp(i32(center.z), 0, 1023);
			zorder_center[i] = Interleave3(cx, cy, cz);

			if (brush.planes.empty())
				continue;
			
			auto& dominant = dominant_material[i];
			dominant = brush.planes[0].material;
			for (auto& usage : material_usage)
				usage = 0;
			
			for (auto& plane : brush.planes) {
				auto& usage = material_usage[plane.material];
				if (++usage > material_usage[dominant])
					dominant = plane.material;
			}

			std::sort(brush.planes.begin(), brush.planes.end(), by_member(&Map::Plane::type));
		}

		std::vector<size_t> order(ent.brushes.size());
		for (size_t i = 0; i < order.size(); ++i)
			order[i] = i;

		std::sort(order.begin(), order.end(), [&] (size_t a, size_t b) {
			auto& brush_a = ent.brushes[a];
			auto& brush_b = ent.brushes[b];

			if (brush_a.axial != brush_b.axial)
				return brush_a.axial > brush_b.axial;

			if (brush_a.planes.size() != brush_b.planes.size())
				return brush_a.planes.size() < brush_b.planes.size();

			if (dominant_material[a] != dominant_material[b])
				return dominant_material[a] < dominant_material[b];

			return zorder_center[a] < zorder_center[b];
		});

		Reorder(ent.brushes, order.data());

		auto last_axial = std::partition(ent.brushes.begin(), ent.brushes.end(), by_member(&Map::Brush::axial));
		//for (auto& brush : range{ent.brushes.begin(), last_axial})
		//	std::sort(brush.planes.begin(), brush.planes.end(), by_member(&Map::Plane::type));
	}

	// Keep brush entities first, point entities last
	std::stable_partition(map.entities.begin() + 1, map.entities.end(), [](Map::Entity& e) { return e.brushes.size() > 0; });
}

////////////////////////////////////////////////////////////////

void RemoveDuplicatePlanes(Map& map) {
	if (map.entities.size() < 2)
		return;

	printf("Removing redundant axial planes\n");

	for (auto& ent : map.entities) {
		for (auto& brush : ent.brushes) {
			for (auto i = brush.planes.begin(); i != brush.planes.end(); /**/) {
				if (i->type == Map::Plane::Unaligned)
					break;
				auto j = i + 1;
				while (j != brush.planes.end() && i->type == j->type)
					++j;

				if (j - i > 1) {
					printf(INDENT "entity %zd, brush %zd: %zd x (%g %g %g) |",
						&ent - map.entities.data(), &brush - ent.brushes.data(), j - i, i->x, i->y, i->z
					);
					float farthest = i->w;
					for (auto duplicate = i; duplicate != j; ++duplicate) {
						if (duplicate->w < farthest)
							farthest = duplicate->w;
						printf(" %g", duplicate->w);
					}
					printf("\n");

					i->w = farthest;
					i = brush.planes.erase(i + 1, j);
				} else {
					i = j;
				}
			}
		}
	}
}

////////////////////////////////////////////////////////////////

template <typename T>
struct SortedArray {
	std::vector<T> items;

	void Add(const T& value) {
		auto i = std::lower_bound(items.begin(), items.end(), value);
		if (i == items.end() || !(*i == value))
			items.insert(i, value);
	}

	int FindIndex(const T& value) const {
		auto i = std::lower_bound(items.begin(), items.end(), value);
		if (i == items.end() || !(*i == value))
			return -1;
		return int(i - items.begin());
	}
};

////////////////////////////////////////////////////////////////

struct UVTransform {
	vec2 offset;
	float angle;
	vec2 scale;

	UVTransform() = default;

	UVTransform(const Map::Plane& plane) :
		offset(plane.shift),
		scale(plane.scale),
		angle(plane.rotation)
	{ }

	bool operator==(const UVTransform& other) const {
		return offset == other.offset && scale == other.scale && angle == other.angle;
	}

	bool operator<(const UVTransform& other) const {
		// trying really hard to avoid tuple/tie...
		float d;
		if ((d = (offset.x - other.offset.x)) != 0.f)	return d < 0.f;
		if ((d = (offset.y - other.offset.y)) != 0.f)	return d < 0.f;
		if ((d = (angle    - other.angle   )) != 0.f)	return d < 0.f;
		if ((d = (scale.x  - other.scale.x )) != 0.f)	return d < 0.f;
		/**/ d = (scale.y  - other.scale.y );        	return d < 0.f;
	}
};

////////////////////////////////////////////////////////////////

void Snap(Map& map, const Options& options) {
	for (auto& entity : map.entities) {
		for (auto& brush : entity.brushes) {
			for (auto& plane : brush.planes) {
				Snap(plane.shift.x,				options.snap.uv_offset);
				Snap(plane.shift.y,				options.snap.uv_offset);
				Snap(plane.rotation,			options.snap.uv_angle);
				SnapMantissa(plane.scale.x,		options.snap.uv_scale_bits);
				SnapMantissa(plane.scale.y,		options.snap.uv_scale_bits);
				
				RemoveNegativeZero(plane.shift.x);
				RemoveNegativeZero(plane.shift.y);
				RemoveNegativeZero(plane.rotation);
				RemoveNegativeZero(plane.scale.x);
				RemoveNegativeZero(plane.scale.y);
			}
		}

		for (auto& patch : entity.patches) {
			vec2 uv_min = patch.vertices[0].uv;
			for (size_t i = 1; i < patch.vertices.size(); ++i)
				uv_min = min(uv_min, patch.vertices[i].uv);
			uv_min = floor(uv_min);
			
			for (auto& v : patch.vertices) {
				v.uv -= uv_min;

				Snap(v.pos.x,				options.snap.patch_vertex);
				Snap(v.pos.y,				options.snap.patch_vertex);
				Snap(v.pos.z,				options.snap.patch_vertex);
				SnapMantissa(v.uv.x,		options.snap.patch_uv_bits);
				SnapMantissa(v.uv.y,		options.snap.patch_uv_bits);
				
				RemoveNegativeZero(v.pos.x);
				RemoveNegativeZero(v.pos.y);
				RemoveNegativeZero(v.pos.z);
				RemoveNegativeZero(v.uv.x);
				RemoveNegativeZero(v.uv.y);
			}
		}
	}
}

////////////////////////////////////////////////////////////////

void RemoveUnneededUVs(Map& map, const Options& options, const ShaderProperties* shader_props) {
	printf("Removing extraneous UV info\n");

	auto& world = map.World();

	size_t num_reset = 0, total = 0;
	for (auto& brush : world.brushes) {
		for (auto& plane : brush.planes) {
			++total;
			auto& props = shader_props[plane.material];
			i32 index = props.map_material;
			if (index < 0)
				index = 0;
			bool needs_uv = Demo::Material::Properties[index] & Demo::Material::NeedsUV;
			if (!needs_uv) {
				plane.scale = 0.5f;
				plane.rotation = 0.f;
				plane.shift = 0.f;
				++num_reset;
			} else if (options.wrap_uvs && props.width > 0 && props.height > 0) {
				plane.shift.x = fmod(plane.shift.x, (float)props.width);
				if (plane.shift.x < -props.width * 0.5f)
					plane.shift.x += props.width;
				else if (plane.shift.x > props.width * 0.5f)
					plane.shift.x -= props.width;
				plane.shift.y = fmod(plane.shift.y, (float)props.height);
				if (plane.shift.y < -props.height * 0.5f)
					plane.shift.y += props.height;
				else if (plane.shift.y > props.height * 0.5f)
					plane.shift.y -= props.height;
			}
		}
	}

	printf(INDENT "reset UV mapping for %zd/%zd planes\n", num_reset, total);
}

////////////////////////////////////////////////////////////////

void DoSetField(i16 (&value)[3], string_view string_value) {
	vec3 v;
	if (string_value.empty() || !ParseVector(string_value, v, false))
		v = 0.f;
	value[0] = i16(floor(v[0] + 0.5f));
	value[1] = i16(floor(v[1] + 0.5f));
	value[2] = i16(floor(v[2] + 0.5f));
}

void DoSetField(i16 &value, string_view string_value) {
	float f;
	if (string_value.empty() || !ParseValue(string_value, f))
		f = 0.f;
	value = i16(floor(f + 0.5f));
}

template <typename T>
void SetField(void* ptr, string_view string_value) {
	DoSetField(*(T*)ptr, string_value);
}

struct FieldDescriptor {
	string_view		name;
	size_t			offset;
	void			(*set)(void*, string_view);
};

static constexpr FieldDescriptor EntityFields[] = {
	#define PP_DEMO_FIELD_DESC(name, type)	{ #name, offsetof(Demo::Entity, name), &SetField<type> },
	DEMO_ENTITY_PROPERTIES(PP_DEMO_FIELD_DESC)
	#undef PP_DEMO_FIELD_DESC
};

void WriteEntities(ArrayPrinter& print, const Map& map, const Options& options, const std::vector<size_t>& entity_brushes, string_view entity_brushes_name, string_view entity_data_name) {
	printf("Writing entities\n");

	/* write entity brush count (for brush entities) */
	print << "const u16 "sv << entity_brushes_name << "[] = {"sv;
	for (auto brush_count : entity_brushes) {
		if (!brush_count)
			break;
		print << u16(brush_count) << ","sv;
	}
	print << "};"sv;
	print.Flush();

	std::unordered_map<string_view, Demo::Entity::Type> classname_to_type;
	#define PP_MAP_CLASSNAME_TO_TYPE(name, ...) classname_to_type[#name] = Demo::Entity::Type::name;
	DEMO_ENTITY_TYPES(PP_MAP_CLASSNAME_TO_TYPE)
	#undef PP_MAP_CLASSNAME_TO_TYPE

	/* gather entity properties */
	std::vector<Demo::Entity> compiled_entities(map.entities.size());
	for (size_t entity_index = 0; entity_index < map.entities.size(); ++entity_index) {
		auto& src_ent = map.entities[entity_index];
		auto& dst_ent = compiled_entities[entity_index];
		dst_ent.type = classname_to_type[src_ent.GetProperty("classname"sv)];
		for (auto& field : EntityFields) {
			auto value = src_ent.GetProperty(field.name);
			field.set(((u8*)&dst_ent) + field.offset, value);
			if (field.name == "angle"sv && !value.empty()) {
				dst_ent.angle = (dst_ent.angle + 270) % 360;
			}
		}
	}

	/* write entity properties (for all entities) */
	print << "\nconst i16 "sv << entity_data_name << "[] = {"sv;
	const size_t NumRawFields = sizeof(Demo::Entity) / sizeof(i16);
	i16* raw_data = (i16*)compiled_entities.data();
	for (size_t field = 0; field < NumRawFields; ++field) {
		for (size_t entity_index = 0; entity_index < compiled_entities.size(); ++entity_index) {
			print << raw_data[field + entity_index * NumRawFields] << ","sv;
		}
	}
	print << "};"sv;
	print.Flush();
}

void WriteBrushBounds(ArrayPrinter& print, const Map& map, const Options& options, string_view bounds_name, string_view array_name) {
	printf("Writing brush bounds\n");

	auto& world = map.World();

	AABB world_bounds;
	world_bounds.mins = floor(world.bounds.mins / 8.f) * 8.f;
	world_bounds.maxs = ceil (world.bounds.maxs / 8.f) * 8.f;
	print << "\nconst i16 "sv << bounds_name << "[6] = {"sv;
	for (u32 i=0; i<6; ++i)
		print << i16(world_bounds.v[i/3].data[i%3]) << ","sv;
	print << "}; // xmin/ymin/zmin/xmax/ymax/zmax"sv;
	print.Flush();

	float lightmap_area = 0.f;
	
	print << "\nconst i16 "sv << array_name << "[] = {"sv;
	for (auto& brush : world.brushes) {
		vec3 offset, size;
		if (true) {
			offset = floor(brush.bounds.mins + 0.5f);
			size = floor(brush.bounds.maxs - offset + 0.5f);
		} else {
			offset = floor(brush.bounds.mins);
			size = ceil(brush.bounds.maxs - offset);
		}
		offset -= world_bounds.mins;

		print
			<< int32_t(offset.x) << ","sv << int32_t(offset.y) << ","sv << int32_t(offset.z) << ","sv
			<< int32_t(size.x) << ","sv << int32_t(size.y) << ","sv << int32_t(size.z) << ","sv;

		AABB lightmap_bounds;
		lightmap_bounds.mins = floor(brush.bounds.mins / 16.f) * 16.f;
		lightmap_bounds.maxs = ceil(brush.bounds.maxs / 16.f) * 16.f;
		lightmap_area += lightmap_bounds.area();
	}
	print << "};"sv;
	print.Flush();

	lightmap_area *= 2.f;	// symmetry
	float lightmap_size = ceil(sqrtf(lightmap_area)) / 16.f;
	printf(INDENT"estimated lightmap size: %.0f x %.0f\n", lightmap_size, lightmap_size);
}

////////////////////////////////////////////////////////////////

void WriteUnalignedPlanes
(
	ArrayPrinter& print, const Map& map, const Options& options,
	string_view plane_count_name, string_view data_array_name, string_view count_array_name
)
{
	printf("Writing unaligned planes\n");

	auto& world = map.World();

	std::vector<int32_t> nonaxial_counts(world.brushes.size());

	size_t max_brush_nonaxial_planes = 0;
	size_t num_unaligned_planes = 0;
	size_t num_planes = 0;
	size_t num_compact_planes = 0;

	print << "\nconst i32 "sv << data_array_name << "[] = {"sv;
	for (size_t brush_index = 0; brush_index < world.brushes.size(); ++brush_index) {
		auto& brush = world.brushes[brush_index];
		size_t num_unaligned_brush_planes = 0;

		const size_t MAX_NUM_EDGES = 256;
		BrushEdge edges[MAX_NUM_EDGES];

		auto brush_center = brush.bounds.center();

		vec3 bounds[2];
		bounds[0] = floor(brush.bounds.mins + 0.5f);
		bounds[1] = floor(brush.bounds.maxs + 0.5f);

		u32 num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), edges, MAX_NUM_EDGES, 1.f);

		for (size_t plane_index = 0; plane_index < brush.planes.size(); ++plane_index) {
			auto& plane = brush.planes[plane_index];
			if (plane.type != Map::Plane::Unaligned)
				continue;

			++num_unaligned_planes;
			++num_planes;
			++num_unaligned_brush_planes;

			int axis0;
			for (axis0 = 0; axis0 < 3; ++axis0)
				if (plane[axis0] == 0.f)
					break;

			if (axis0 != 3) {
				const u32
					OffsetBits		= 12,
					OffsetMask		= (1 << OffsetBits) - 1
				;

				int axis1 = (axis0 + 1) % 3;
				int axis2 = (axis0 + 2) % 3;
				bool negative = plane[axis0] < 0.f;
				
				vec3 corner;
				corner[0] = bounds[plane[0] >= 0.f][0];
				corner[1] = bounds[plane[1] >= 0.f][1];
				corner[2] = bounds[plane[2] >= 0.f][2];

				float dist = dot(corner, plane.xyz) + plane.w;
				float offset1 = std::abs(dist / plane[axis1]);
				float offset2 = std::abs(dist / plane[axis2]);
				float rounded_offset1 = std::round(offset1);
				float rounded_offset2 = std::round(offset2);

				const float Tolerance = 1e-3f;
				if (std::abs(offset1 - rounded_offset1) < Tolerance &&
					std::abs(offset2 - rounded_offset2) < Tolerance &&
					std::max(offset1, offset2) <= float(OffsetMask)
				) {
					// compact encoding
					++num_compact_planes;
					i32 value =
						axis0
						| ((plane[axis1] < 0.f) << 2)
						| ((plane[axis2] < 0.f) << 3)
						| (i32(rounded_offset1) << 4)
						| (i32(rounded_offset2) << 16)
						;
					print << value << ","sv;
					continue;
				}
			}

			const i32
				OctBits			= 12,
				OctMask			= (1 << OctBits) - 1,
				OctMaxValue		= OctMask >> 1,
				DistFractBits	= 4,
				DistScale		= 1 << DistFractBits;

			vec2 oct = vec3_to_oct(plane.xyz);
			i32 x = EncodeSignMagnitude(std::lround(oct.x * OctMaxValue));
			i32 y = EncodeSignMagnitude(std::lround(oct.y * OctMaxValue));
			u32 xy = x | (y << 16);

			oct.x = DecodeSignMagnitude(x) / (float)OctMaxValue;
			oct.y = DecodeSignMagnitude(y) / (float)OctMaxValue;
			vec3 snapped_normal = oct_to_vec3(oct);
			float w = plane.w;

			vec3 face_center{0.f};
			u32 num_face_edges = 0;
			for (u32 edge_index = 0; edge_index < num_edges; ++edge_index) {
				auto& e = edges[edge_index];
				if (e.first_plane == plane_index || e.second_plane == plane_index) {
					face_center += e.first_point;
					face_center += e.second_point;
					++num_face_edges;
				}
			}
			if (num_face_edges > 0)
				face_center /= float(num_face_edges * 2);
			else
				face_center = brush_center;

			vec3 projected_center = face_center - plane.xyz * (dot(plane.xyz, face_center) + plane.w);
			w = -dot(projected_center, snapped_normal);

			print
				<< i32((xy << 2) | 3) << ","sv
				<< (i32)EncodeSignMagnitude(std::lround(w * DistScale)) << ","sv
			;
		}

		max_brush_nonaxial_planes = std::max(max_brush_nonaxial_planes, num_unaligned_brush_planes);
		nonaxial_counts[brush_index] = num_unaligned_brush_planes;
	}

	print << "};"sv;
	print.Flush();
	
	print << "\nconst u16 "sv << count_array_name << "[] = {"sv;
	for (auto count : nonaxial_counts)
		print << count << ","sv;
	print << "};"sv;
	print.Flush();

	print << "\nconst u16 "sv << plane_count_name << " = "sv << i32(num_unaligned_planes) << ";"sv;
	print.Flush();

	printf(INDENT "%zd unaligned planes, %zd of which compact (%.1f%%)\n",
		num_planes, num_compact_planes, float(num_compact_planes) * 100.f / float(num_planes)
	);
}

////////////////////////////////////////////////////////////////

static const char* const MaterialSubstitutions[][2] = {
	#define PP_MATERIAL_SUBSTITION(before, after) {before, after},
	DEMO_MATERIAL_SUBSTITUTIONS(PP_MATERIAL_SUBSTITION)
	#undef PP_MATERIAL_SUBSTITION
};

void ApplyMaterialSubstitutions(Map& map) {
	printf("Applying materials substitutions\n");

	std::vector<size_t> replacement(map.materials.size());
	for (size_t i = 0; i < replacement.size(); ++i)
		replacement[i] = i;

	for (auto& subst : MaterialSubstitutions) {
		auto before = map.FindMaterial(subst[0]);
		auto after = map.AddMaterial(subst[1]);
		if (before != -1 && after != -1) {
			replacement[before] = after;
		}
	}

	for (auto& entity : map.entities) {
		for (auto& brush : entity.brushes) {
			for (auto& plane : brush.planes)
				if (plane.material != -1)
					plane.material = replacement[plane.material];
		}
		for (auto& patch : entity.patches) {
			if (patch.material != -1)
				patch.material = replacement[patch.material];
		}
	}
}

void WriteMaterials(ArrayPrinter& print, const Map& map, const Options& options, const ShaderProperties* shader_props, string_view count_name, string_view array_name) {
	auto& world = map.World();

	printf("Writing plane materials\n");

	/* sort materials by usage for printing */
	std::vector<size_t> usage(map.materials.size());
	for (auto& brush : world.brushes)
		for (auto& plane : brush.planes)
			++usage[plane.material];
	for (auto& patch : world.patches)
		++usage[patch.material];

	std::vector<size_t> order(map.materials.size());
	for (size_t i = 0; i < order.size(); ++i)
		order[i] = i;

	std::sort(order.begin(), order.end(), [&] (size_t a, size_t b) {
		return usage[a] > usage[b];
	});

	size_t mapped = 0;
	for (size_t i = 0; i < map.materials.size(); ++i)
		if (shader_props[i].map_material >= 0)
			++mapped;

	/* report unmapped shaders */
	if (mapped != map.materials.size()) {
		DebugPrint("Unmapped shaders:\n");
		DebugPrint("------------------------\n");
		
		for (auto mat_index : order) {
			auto& material = map.materials[mat_index];
			auto& value = shader_props[mat_index].map_material;
			if (value < 0) {
				printf(INDENT "defaulting %s (%zdx)\n", material.name.c_str(), usage[mat_index]);
				DebugPrint("%s (%zdx)\n", material.name.c_str(), usage[mat_index]);
			}
		}
	}

	/* write plane materials + uv axes */
	print
		<< "\nconst u8 "sv << count_name << " = "sv << i32(std::size(DemoMaterialNames)) << ", "sv
		<< array_name << "[] = {"sv;
	for (auto& brush : world.brushes) {
		for (auto& plane : brush.planes) {
			i32 index = shader_props[plane.material].map_material;
			if (index < 0)
				index = 0;
			index = (index << 2) | dominant_axis(plane.xyz);
			print << index << ","sv;
		}
	}
	print << "};"sv;
	print.Flush();
}

////////////////////////////////////////////////////////////////

void WriteBrushUVs(ArrayPrinter& print, const Map& map, const Options& options, const ShaderProperties* shader_props, string_view uv_set_name, string_view plane_map_name) {
	printf("Writing UVs\n");

	auto& world = map.World();

	SortedArray<UVTransform> plane_uvs;

	for (auto& brush : world.brushes) {
		for (auto& plane : brush.planes) {
			if (!(shader_props[plane.material].surface_flags & Surface::NODRAW)) {
				plane_uvs.Add(plane);
			}
		}
	}

	printf(INDENT "%zd unique UV combinations\n", plane_uvs.items.size());

	std::vector<i32> usage(plane_uvs.items.size());
	if (options.sort_uv == Options::UVSort::ByUsage) {
		for (auto& brush : world.brushes) {
			for (auto& plane : brush.planes)
				if (!(shader_props[plane.material].surface_flags & Surface::NODRAW))
					++usage[plane_uvs.FindIndex(plane)];
		}
	} else if (options.sort_uv == Options::UVSort::ByMapOrder) {
		for (auto& u : usage)
			u = -1;

		i32 cursor = 0;
		for (auto& brush : world.brushes) {
			for (auto& plane : brush.planes) {
				if (!(shader_props[plane.material].surface_flags & Surface::NODRAW)) {
					auto index = plane_uvs.FindIndex(plane);
					if (usage[index] == -1) {
						usage[index] = cursor++;
					}
				}
			}
		}
		for (auto& u : usage)
			u = -1;
	}

	std::vector<size_t> order(plane_uvs.items.size());
	for (size_t i = 0; i < order.size(); ++i)
		order[i] = i;

	if (options.sort_uv != Options::UVSort::None) {
		std::sort(order.begin(), order.end(), [&] (size_t a, size_t b) {
			return usage[a] > usage[b];
		});
	}

	std::vector<size_t> remap(order.size());
	for (size_t i = 0; i < order.size(); ++i)
		remap[order[i]] = i;

	print << "\nconst float "sv << uv_set_name << "[] = {"sv;
	for (int i=0; i<5; ++i) {
		//for (auto& uv : plane_uvs.items) {
		for (auto uv_index : order) {
			auto& uv = plane_uvs.items[uv_index];
			//print << uv.offset.x << ","sv << uv.offset.y << ","sv << uv.angle << ","sv << uv.scale.x << ","sv << uv.scale.y << ","sv;
			switch (i) {
			case 0:
			case 1: print << uv.offset.data[i] << ","sv; break;
			case 2: print << uv.angle << ","sv; break;
			case 3:
			case 4: print << uv.scale.data[i-3] << ","sv; break;
			default:
				break;
			}
		}
	}
	print << "};"sv;
	print.Flush();

	assert(plane_uvs.items.size() <= 256);

	i32 last_index = 0;
	print << "\nconst u8 "sv << plane_map_name << "[] = {"sv;
	for (auto& brush : world.brushes) {
		for (auto& plane : brush.planes) {
			i32 index = plane_uvs.FindIndex(plane);
			if (index == -1 || shader_props[plane.material].surface_flags & Surface::NODRAW) {
				index = last_index;
			} else {
				last_index = index;
				index = remap[index];
			}
			print << index << ","sv;
		}
	}
	print << "};"sv;
	print.Flush();
}

////////////////////////////////////////////////////////////////

void SafeNormalize(vec3& v) {
	float l = length(v);
	if (l > 0.f)
		v /= l;
}

float LineDistance(const vec3& p, const vec3& a, const vec3& b) {
	vec3 ab = b - a, ap = p - a;
	SafeNormalize(ab);
	return length(ap - ab * dot(ab, ap));
}

void WritePatchData(ArrayPrinter& print, const Map& map, const Options& options, const ShaderProperties* props, string_view patch_array_name, string_view vert_array_name) {
	printf("Writing patches\n");

	auto& world = map.entities[0];

	u32 max_width = 0, max_height = 0;
	u32 num_control_points = 0;
	u32 num_tris = 0, num_verts = 0;
	for (auto& patch : world.patches) {
		max_width = std::max(max_width, patch.width);
		max_height = std::max(max_height, patch.height);
		num_control_points += patch.width * patch.height;
	}
	printf(INDENT "max patch size: %d x %d\n", max_width, max_height);

	std::vector<size_t> order(world.patches.size());
	for (size_t i = 0; i < order.size(); ++i)
		order[i] = i;

	if (options.sort_patches) {
		auto mins = world.bounds.mins;
		auto size = world.bounds.size();

		std::vector<u64> sort_keys(world.patches.size());
		for (size_t i = 0; i < sort_keys.size(); ++i) {
			auto& patch = world.patches[i];
			auto center = patch.bounds.center();
			auto x = std::clamp(i32((center.x - mins.x) / size.x * 1023.f + 0.5f), 0, 1023);
			auto y = std::clamp(i32((center.y - mins.y) / size.y * 1023.f + 0.5f), 0, 1023);
			auto z = std::clamp(i32((center.z - mins.z) / size.z * 1023.f + 0.5f), 0, 1023);
			sort_keys[i] = Interleave3(x, y, z) | (u64(patch.material) << 30);
		}

		std::sort(order.begin(), order.end(), [&] (size_t a, size_t b) {
			return sort_keys[a] < sort_keys[b];
		});

		for (size_t i = 0; i < order.size(); ++i) {
			printf("Sort: %lld\n", sort_keys[order[i]]);
		}
	}

	print << "\nconst u32 "sv << patch_array_name << "[] = {"sv;
	for (auto patch_index : order) {
		auto& patch = world.patches[patch_index];
		vec2 max_dist = 0.f;

		auto* ctrl = patch.vertices.data();
		for (i16 j = 0; j < patch.height; ++j) {
			for (i16 i = 0; i < patch.width; ++i, ++ctrl) {
				if (i & 1)
					max_dist.x = std::max(max_dist.x, LineDistance(ctrl[0].pos, ctrl[-1].pos, ctrl[1].pos));
				if (j & 1)
					max_dist.y = std::max(max_dist.y, LineDistance(ctrl[0].pos, ctrl[-(i32)patch.width].pos, ctrl[patch.width].pos));
			}
		}

		const i32
			SizeBits			= 3,
			MaxPatchSize		= (((1 << SizeBits) - 1) << 1) + 3,
			MaterialBits		= 6,
			MaxMaterial			= (1 << MaterialBits) - 1,
			MaxLevel			= 4,
			MaxCombinedLevels	= 6;

		// TODO: better LOD model
		auto lod_level = [](float dist) {
			return std::max((int)(log2f(std::max(dist, 1.f)) - 1.5f), 0);
		};

		i16 material	= std::max<i16>(props[patch.material].map_material, 0);
		u32 width		= (patch.width - 3) >> 1;
		u32 height		= (patch.height - 3) >> 1;
		i32 divx		= lod_level(max_dist.x);
		i32 divy		= lod_level(max_dist.y);

		// clamp level of detail
		i32 overflow	= std::max(divx + divy - MaxCombinedLevels, 0) >> 1;
		if (options.verbose && overflow > 0)
			printf(INDENT "clamping patch divs: %d %d\n", divx, divy);
		divx = std::clamp(divx - overflow, 0, MaxLevel);
		divy = std::clamp(divy - overflow, 0, MaxLevel);

		u32 packed	= width | (height << 3) | (divx << 6) | (divy << 9) | (material << 12);

		assert(patch.width >= 3);
		assert(patch.width <= MaxPatchSize);
		assert(patch.width & 1);
		assert(patch.height >= 3);
		assert(patch.height <= MaxPatchSize);
		assert(patch.height & 1);
		assert(material <= MaxMaterial);

		print << i32(packed) << ","sv;

		u8 prim_x	= patch.width >> 1;
		u8 prim_y	= patch.height >> 1;
		u32 verts_x	= (prim_x << divx) + 1;
		u32 verts_y	= (prim_y << divy) + 1;

		num_verts	+= verts_x * verts_y;
		num_tris	+= (verts_x - 1) * (verts_y - 1) * 2;
	}
	print << "};"sv;
	print.Flush();

	print << "\nconst i16 "sv << vert_array_name << "[] = {"sv;
	for (int i=0; i<5; ++i) {
		for (auto patch_index : order) {
			auto& patch = world.patches[patch_index];
			vec2 uv_base = patch.vertices[0].uv;
			for (auto& v : patch.vertices)
				uv_base = min(uv_base, v.uv);
			uv_base = floor(uv_base);

			i16 prev = 0, value = 0;
			for (auto& v : patch.vertices) {
				switch (i) {
				case 0:
				case 1:
				case 2:
					value = std::lround(v.pos.data[i]); break;
				case 3:
				case 4:
					value = std::lround((v.uv.data[i-3] - uv_base[i-3]) * 256.f); break;
				default:
					assert(false);
					continue;
				}

				print << (i16)EncodeSignMagnitude(value - prev) << ","sv;
				prev = value;
			}
		}
	}
	print << "};"sv;
	print.Flush();

	printf(INDENT "%zd patches, %d ctl. points, %d verts, %d tris\n",
		world.patches.size(), num_control_points, num_verts, num_tris);
}

////////////////////////////////////////////////////////////////

u32 ReadSkyLight(const Map& map) {
	auto prop = map.World().GetProperty("_skylight");
	i32 r, g, b;
	if (!ParseValue(prop, r))
		return 0;
	if (!ParseValue(prop, g) || !ParseValue(prop, b))
		g = b = r;
	return r | (g << 8) | (b << 16);
}

void WriteLights
(
	ArrayPrinter& print, const Map& map, const Options& options, const ShaderProperties* shader_props,
	const std::vector<Map::Entity>& light_entities,
	const Symmetry& symmetry,
	string_view array_name, string_view spot_count_name
)
{
	printf("Writing lights\n");

	auto& world = map.World();

	std::unordered_map<std::string_view, size_t> named_entity;
	named_entity.reserve(map.entities.size());
	for (auto& ent : map.entities) {
		auto targetname = ent.GetProperty("targetname"sv);
		if (!targetname.empty())
			named_entity.insert({targetname, &ent - map.entities.data()});
	}

	SortedArray<float> light_values;
	light_values.items.reserve(map.entities.size());

	struct Light {
		enum {
			IsSpotlight		= 1 << 30,
		};

		float	intensity	= 300.f;
		vec3	pos			= 0.f;
		vec3	color		= 1.f;
		vec3	spot		= 0.f;
		u32		key			= 0;
	};

	std::vector<Light> lights;
	lights.reserve(256);

	// first light is always the sun
	auto& sun = lights.emplace_back();
	auto sun_prop = world.GetProperty("_sun");
	float sun_degrees = 0.f;
	float sun_elevation = 0.f;

	bool valid_sun =
		ParseVector(sun_prop, sun.color, false) &&
		ParseValue(sun_prop, sun.intensity) &&
		ParseValue(sun_prop, sun_degrees) &&
		ParseValue(sun_prop, sun_elevation);

	if (valid_sun) {
		const float Distance = 8192.f;
		float sin_yaw = sin(sun_degrees * Math::DEG2RAD);
		float cos_yaw = cos(sun_degrees * Math::DEG2RAD);
		float sin_pitch = sin(sun_elevation * Math::DEG2RAD);
		float cos_pitch = cos(sun_elevation * Math::DEG2RAD);
		sun.pos.x = Distance * cos_yaw * cos_pitch;
		sun.pos.y = Distance * sin_yaw * cos_pitch;
		sun.pos.z = Distance * sin_pitch;
	} else {
		sun = {};
		sun.intensity = 0.f;
	}

	size_t num_spotlights = 0;
	for (size_t i = 0; i < light_entities.size(); ++i) {
		auto& ent = light_entities[i];

		Light light;

		auto origin_str = ent.GetProperty("origin"sv);
		if (origin_str.empty() || !ParseVector(origin_str, light.pos, false)) {
			printf(INDENT "warning: light entity %zd with no origin\n", i);
			continue;
		}

		if (symmetry && light.pos[symmetry.axis] > symmetry.level + 1.f)
			continue;

		auto target_str = ent.GetProperty("target"sv);
		if (!target_str.empty()) {
			if (!options.use_spotlights)
				continue;

			auto i = named_entity.find(target_str);
			if (i == named_entity.end()) {
				printf(INDENT "warning: entity %zd spotlight target '%*s' not found\n", i->second, int(target_str.size()), target_str.data());
				continue;
			}

			auto& target = map.entities[i->second];
			auto target_origin_str = target.GetProperty("origin"sv);
			vec3 target_pos;
			if (target_origin_str.empty() || !ParseVector(target_origin_str, target_pos, false)) {
				printf(INDENT "warning: entity %zd spotlight target '%*s' has no origin\n", i->second, int(target_str.size()), target_str.data());
				continue;
			}

			float radius;
			auto radius_str = ent.GetProperty("radius");
			if (!ParseValue(radius_str, radius))
				radius = 64.f;

			light.spot = (target_pos - light.pos) * (64.f / radius);
			light.key |= Light::IsSpotlight;
			++num_spotlights;
		}

		auto light_str = ent.GetProperty("light"sv);
		if (light_str.empty() || !ParseValue(light_str, light.intensity))
			light.intensity = 300.f;

		auto color_str = ent.GetProperty("_color"sv);
		if (color_str.empty() || !ParseVector(color_str, light.color, false))
			light.color = 1.f;
		float max_value = std::max(light.color.r, std::max(light.color.g, light.color.b));
		if (max_value != 0.f)
			light.color /= max_value;

		light_values.Add(light.intensity);
		lights.push_back(light);
	}

	size_t num_area_lights = 0;
	if (options.use_area_lights) {
		std::vector<size_t> face_edges;
		std::vector<vec3> face_corners;
		face_edges.reserve(64);
		face_corners.reserve(64);

		for (auto& brush : world.brushes) {
			auto brush_index = &brush - world.brushes.data();

			const size_t MAX_NUM_EDGES = 256;
			BrushEdge brush_edges[MAX_NUM_EDGES];

			size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), brush_edges, MAX_NUM_EDGES);

			const bool Snap = false;
			if (Snap) {
				size_t num_valid_edges = 0;
				for (size_t edge_index = 0; edge_index < num_edges; ++edge_index) {
					auto& edge = brush_edges[edge_index];
					edge.first_point = floor(edge.first_point + 0.5f);
					edge.second_point = floor(edge.second_point + 0.5f);
					if (length_squared(edge.second_point - edge.first_point) > 0.25f)
						brush_edges[num_valid_edges++] = edge;
				}
				num_edges = num_valid_edges;
			}

			for (auto& plane : brush.planes) {
				auto material = shader_props[plane.material].map_material_base;
				if (material < 0)
					continue;

				auto emissive = Demo::Material::UnpackSurfaceLight(Demo::Material::Lights[material]);
				if (emissive.intensity == 0)
					continue;

				auto plane_index = &plane - brush.planes.data();

				vec3 center = 0.f;
				face_edges.clear();
				for (size_t edge_index = 0; edge_index < num_edges; ++edge_index) {
					auto& edge = brush_edges[edge_index];
					if (edge.first_plane != plane_index && edge.second_plane != plane_index)
						continue;
					face_edges.push_back(edge_index);
					center += (edge.first_point + edge.second_point);
				}

				if (face_edges.size() < 3)
					continue;

				center /= face_edges.size() * 2;
				vec3 x_axis = normalize(brush_edges[face_edges[0]].first_point - center);
				vec3 y_axis = cross(x_axis, plane.xyz);

				auto get_edge_angle = [&] (size_t index) {
					const auto& e = brush_edges[index];
					vec3 delta = (e.second_point + e.first_point) * 0.5f - center;
					return atan2(dot(delta, y_axis), dot(delta, x_axis));
				};

				std::sort(face_edges.begin(), face_edges.end(), [&] (size_t a, size_t b) {
					return get_edge_angle(a) < get_edge_angle(b);
				});

				face_corners.clear();
				for (auto index : face_edges) {
					auto& e = brush_edges[index];
					face_corners.push_back(e.first_plane == plane_index ? e.first_point : e.second_point);
				}

				const float
					AreaScale		= 0.25f / 7500.f,
					SurfaceOffset	= 4.f
				;

				float area = 0.f;
				for (size_t corner_index = 2; corner_index < face_corners.size(); ++corner_index) {
					const vec3& a = face_corners[corner_index - 2];
					const vec3& b = face_corners[corner_index - 1];
					const vec3& c = face_corners[corner_index - 0];
					float ab = length(b - a);
					float bc = length(c - b);
					float ca = length(a - c);
					float p = (ab + bc + ca) * 0.5f;
					area += sqrtf(p * (p - ab) * (p - bc) * (p - ca));
				}

				auto& light = lights.emplace_back();
				light.pos = floor(center + SurfaceOffset * plane.xyz + 0.5f);
				light.intensity = area * AreaScale * emissive.intensity;
				SnapMantissa(light.intensity, 21);
				light.color = emissive.color;

				const bool AreaLightsAreSpotlights = false;
				if (AreaLightsAreSpotlights) {
					light.key |= Light::IsSpotlight;
					light.spot = floor(plane.xyz * 8.f + 0.5f);
					++num_spotlights;
				}

				if (options.verbose)
					printf(INDENT"Surface light: %s (%g %g %g) %g\n",
						map.materials[plane.material].name.c_str(), light.pos.x, light.pos.y, light.pos.z, light.intensity);

				++num_area_lights;
			}
		}
	}

	if (options.sort_lights) {
		auto mins = world.bounds.mins;
		auto center = world.bounds.center();
		auto size = world.bounds.size();

		for (auto& light : lights) {
			auto x = std::clamp(i32((light.pos.x - mins.x) / size.x * 1023.f + 0.5f), 0, 1023);
			auto y = std::clamp(i32((light.pos.y - mins.y) / size.y * 1023.f + 0.5f), 0, 1023);
			auto z = std::clamp(i32((light.pos.z - mins.z) / size.z * 1023.f + 0.5f), 0, 1023);
			light.key |= Interleave3(x, z, y);
		}

		// keep the sun at index 0, sort the rest
		assert(lights.size() >= 1);
		std::sort(lights.begin() + 1, lights.end(), by_member(&Light::key));
	}
	
	print << "\nconst i16 "sv << array_name << "[] = {"sv;
	for (auto& light : lights) {
		u16 ldr =
			(std::clamp<u8>(light.color.x * 31.f + 0.5f, 0, 31) << 0) |
			(std::clamp<u8>(light.color.y * 31.f + 0.5f, 0, 31) << 5) |
			(std::clamp<u8>(light.color.z * 31.f + 0.5f, 0, 31) << 10) ;

		print
			<< i16(floor(light.pos.x + 0.5f)) << ","sv
			<< i16(floor(light.pos.y + 0.5f)) << ","sv
			<< i16(floor(light.pos.z + 0.5f)) << ","sv
			<< i16(floor(light.intensity + 0.5f)) << ","sv
			<< i16(ldr) << ","sv
		;
	}
	for (auto& light : lights) {
		if (!(light.key & Light::IsSpotlight))
			continue;
		print
			<< i16(floor(light.spot.x + 0.5f)) << ","sv
			<< i16(floor(light.spot.y + 0.5f)) << ","sv
			<< i16(floor(light.spot.z + 0.5f)) << ","sv
		;
	}
	print << "};"sv;
	print.Flush();

	print << "const u8 "sv << spot_count_name << " = "sv << i32(num_spotlights) << ";"sv;
	print.Flush();

	printf(INDENT "%zd lights (%zd surface lights, %zd spotlights), %zd unique light values\n",
		lights.size(), num_area_lights, num_spotlights, light_values.items.size()
	);
}

////////////////////////////////////////////////////////////////

void WriteLightmap(ArrayPrinter& print, const Map& map, const Options& options, const ShaderProperties* shader_props, string_view array_name) {
	printf("Writing lightmap\n");

	const u16
		LightmapWidth		= 384,
		LightmapHeight		= 256,
		RoundTo				= 1,
		LightmapScale		= 16
	;
	static_assert((RoundTo & (RoundTo - 1)) == 0, "RoundTo must be a power of 2");

	auto& world = map.World();

	struct LitSurface {
		using TileID = RectPacker::Index;

		i16		brush = -1;
		i8		plane = -1;
		u16		width = 0;
		u16		height = 0;
		i16		x = 0;
		i16		y = 0;
		TileID	tile = RectPacker::Full;
	};
	std::vector<LitSurface> surfaces;
	surfaces.reserve(2048);

	auto needs_lightmap = [&] (i32 material) {
		material = shader_props[material].map_material;
		if (material < 0)
			material = 0;
		auto props = Demo::Material::Properties[material];
		return (props & Demo::Material::MaskVisibility) == Demo::Material::Opaque;
	};

	float area = 0.f;
	size_t num_lit_surfs = 0, num_surfs = 0;

	for (auto& brush : world.brushes) {
		auto brush_index = &brush - world.brushes.data();

		const size_t MAX_NUM_EDGES = 256;
		BrushEdge brush_edges[MAX_NUM_EDGES];

		size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), brush_edges, MAX_NUM_EDGES);

		for (auto& plane : brush.planes) {
			++num_surfs;
			if (!needs_lightmap(plane.material))
				continue;
			++num_lit_surfs;
			
			Rect uv_bounds;
			uv_bounds.clear();

			auto uv_axis = dominant_axis(plane.xyz);
			auto uv_map = [&] (const vec3& p) {
				switch (uv_axis) {
					case 0: return vec2{p[1], p[2]};
					case 1: return vec2{p[0], p[2]};
					default:
					case 2: return vec2{p[0], p[1]};
				}
			};

			auto plane_index = &plane - brush.planes.data();
			for (size_t i = 0; i < num_edges; ++i) {
				auto& edge = brush_edges[i];
				if (edge.first_plane == plane_index || edge.second_plane == plane_index) {
					uv_bounds.add(uv_map(edge.first_point));
					uv_bounds.add(uv_map(edge.second_point));
				}
			}

			uv_bounds.mins = floor(uv_bounds.mins / LightmapScale);
			uv_bounds.maxs = ceil(uv_bounds.maxs / LightmapScale);

			auto& surf = surfaces.emplace_back();
			surf.brush = brush_index;
			surf.plane = plane_index;
			surf.x = uv_bounds.mins.x;
			surf.y = uv_bounds.mins.y;
			surf.width = uv_bounds.size().x + 1;
			surf.height = uv_bounds.size().y + 1;

			area += surf.width * surf.height;
		}
	}

	if (0)
	std::sort(surfaces.begin(), surfaces.end(), [](LitSurface& a, LitSurface& b) {
		i16 d;
		d = a.width - b.width;
		if (d > 0) return true;
		if (d < 0) return false;
		return a.height > b.height;
	});

	i32 base_ofs[2] = {0x7fff'ffff, 0x7fff'ffff};
	RectPacker packer(LightmapWidth, LightmapHeight, 2048);
	for (auto& surf : surfaces) {
		const auto RoundBias = RoundTo - 1;
		surf.tile = packer.Add((surf.width + RoundBias) & ~RoundBias, (surf.height + RoundBias) & ~RoundBias);
		assert(surf.tile != packer.Full);
		auto& r = packer.GetTile(surf.tile);
		Math::assign_min(base_ofs[0], r.min[0] - surf.x);
		Math::assign_min(base_ofs[1], r.min[1] - surf.y);
	}

	i32 max_ofs[2] = {std::numeric_limits<i32>::min(), std::numeric_limits<i32>::min()};
	for (auto& surf : surfaces) {
		auto& r = packer.GetTile(surf.tile);
		Math::assign_max(max_ofs[0], r.min[0] - surf.x - base_ofs[0]);
		Math::assign_max(max_ofs[1], r.min[1] - surf.y - base_ofs[1]);
	}

	print << "\nconst u16 "sv << array_name << "[] = {"sv;
	i32 last_value = 0;
	for (int pass = 0; pass < 2; ++pass) {
		size_t surface_index = 0;
		for (auto& brush : world.brushes) {
			auto brush_index = &brush - world.brushes.data();

			const size_t MAX_NUM_EDGES = 256;
			BrushEdge brush_edges[MAX_NUM_EDGES];

			size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), brush_edges, MAX_NUM_EDGES);

			for (auto& plane : brush.planes) {
				if (!needs_lightmap(plane.material)) {
					print << last_value << ","sv;
					continue;
				}

				auto& surf = surfaces[surface_index++];
				auto& r = packer.GetTile(surf.tile);

				switch (pass) {
					case 0:	print << (last_value = (r.min[0] - surf.x - base_ofs[0])) << ","sv; break;
					case 1: print << (last_value = (r.min[1] - surf.y - base_ofs[1])) << ","sv; break;
					default:
						break;
				}
			}
		}
		assert(surface_index == surfaces.size());
	}
	print << "};"sv;
	print.Flush();

	float min_size = ceil(sqrt(area));

	printf(INDENT"%zd/%zd lit planes\n", num_lit_surfs, num_surfs);
	printf(INDENT"%dx%d lightmap size, %.1f%% usage\n", packer.GetWidth(), packer.GetHeight(), packer.GetUsedTexels() * 100.f / (packer.GetWidth() * packer.GetHeight()));
}

////////////////////////////////////////////////////////////////

struct Levelshot {
	i16 position[3] = {0, 0, 0};
	i16 angles[2] = {0, 0};

	bool Parse(std::string_view property) {
		return
			ParseValue(property, position[0]) &&
			ParseValue(property, position[1]) &&
			ParseValue(property, position[2]) &&
			ParseValue(property, angles[0]) &&
			ParseValue(property, angles[1]) ;
	}

	bool FindInMap(const Map& map) {
		if (Parse(map.World().GetProperty("_levelshot"sv)))
			return true;
		*this = {};
		return false;
	}
};

////////////////////////////////////////////////////////////////

bool CompileMap(Map& map, const char* name, const char* source_name, const Options& options, FILE* out) {
	if (map.entities.empty())
		return false;

	ApplyMaterialSubstitutions(map);
	MergeFuncGroups(map);
	map.ComputeBounds();

	Symmetry symmetry;
	ExploitSymmetry(map, options, symmetry);

	if (options.reorder_materials)
		SortMaterialsByUsage(map, options);

	auto shader_props = GetShaderProperties(map, options);
	//RemoveSkyNodropBrushes(map, shader_props.data());
	map.ComputeBounds();

	InsertMissingAxialPlanes(map);
	SortBrushesAndPlanes(map);

	RebuildEntityLinks(map, options);
	std::vector<Map::Entity> lights;
	RemoveUnknownEntities(map, options, lights);
	SortEntities(map, options);

	std::vector<size_t> brush_count;
	MergeEntityBrushes(map, options, brush_count);

	RemoveDuplicatePlanes(map);
	Snap(map, options);
	if (options.trim_unwanted_uvs)
		RemoveUnneededUVs(map, options, shader_props.data());

	auto& world = map.entities[0];

	/* header */
	std::string description{world.GetProperty("message"sv)};
	fprintf(out,
		"\n"
		"////////////////////////////////////////////////////////////////\n"
		"// %.*s (%s)\n"
		"////////////////////////////////////////////////////////////////\n"
		"\n"
		"namespace %s {\n",
		int(description.size()), description.c_str(), source_name,
		name
	);

	ArrayPrinter print(out);

	WriteEntities			(print, map, options, brush_count,								"entity_brushes"sv,			"entity_data"sv			);
	WriteBrushBounds		(print, map, options,											"world_bounds"sv,			"brush_bounds"sv		);
	WriteUnalignedPlanes	(print, map, options,											"num_nonaxial_planes"sv,	"nonaxial_planes"sv,	"nonaxial_counts"sv);
	WriteMaterials			(print, map, options, shader_props.data(),						"num_materials"sv,			"plane_materials"sv		);
	WriteBrushUVs			(print, map, options, shader_props.data(),						"uv_set"sv,					"plane_uvs"sv			);
	WritePatchData			(print, map, options, shader_props.data(),						"patches"sv,				"patch_verts"sv			);
	WriteLights				(print, map, options, shader_props.data(),	lights,	symmetry,	"light_data"sv,				"num_spotlights"sv		);
	//WriteLightmap			(print, map, options, shader_props.data(),	"lightmap_offsets"										);

	/* footer */
	Levelshot levelshot;
	bool has_levelshot = levelshot.FindInMap(map);
	assert(has_levelshot);

	for (auto& c : description)
		c = std::toupper(c);
	ReplaceAll(description, "\\",  "\\\\");
	ReplaceAll(description, "\"",  "\\\"");

	fprintf(out,
		"\n"
		"static constexpr PackedMap map{\n"
		"    \"%s\",\n"
		"    %d, %d, // symmetry axis, level\n"
		"    entity_brushes, entity_data,\n"
		"    world_bounds, brush_bounds,\n"
		"    num_nonaxial_planes, nonaxial_planes, nonaxial_counts,\n"
		"    num_materials, plane_materials,\n"
		"    uv_set, plane_uvs,\n"
		"    patches, patch_verts,\n"
		"    light_data, num_spotlights,\n"
		"    0x%06x, // skylight\n"
		"    %d, %d, %d, // levelshot position\n"
		"    %d, %d // levelshot yaw, pitch\n"
		"};\n",
		description.c_str(),
		symmetry.axis, symmetry.level,
		ReadSkyLight(map),
		levelshot.position[0], levelshot.position[1], levelshot.position[2],
		levelshot.angles[0], levelshot.angles[1]
	);
	fprintf(out, "} // namespace %s\n", name);

	return true;
}

////////////////////////////////////////////////////////////////

struct MapEntry {
	const char* map_name;
	const char* src_file_name;
	const char* src_path;
};

static constexpr MapEntry DemoMaps[] = {
	#define PP_DEMO_MAP_DEF(name, source, ...)		{#name, #source, "../../demo/data/" #source},
	DEMO_MAPS(PP_DEMO_MAP_DEF)
	#undef PP_DEMO_MAP_DEF
};

////////////////////////////////////////////////////////////////

int main() {
	Options options;
	options.out_path			= "../../demo/cooked/cooked_maps.h";
	options.snap.uv_angle		= 1.f;
	options.snap.uv_offset		= 2.f;
	options.snap.uv_scale_bits	= 16;
	options.snap.patch_uv_bits	= 12;
	options.snap.patch_vertex	= 1.f;
	options.sort_uv				= Options::UVSort::ByMapOrder;
	options.reorder_materials	= false;
	options.trim_unwanted_uvs	= true;		// saves 1k!
	options.wrap_uvs			= false;	// minor net loss
	options.sort_lights			= true;
	options.sort_patches		= false;
	options.verbose				= false;

	FILE* out = fopen(options.out_path, "w");
	if (!out)
		return 1;
	auto close_output = scope_exit { fclose(out); out = nullptr; };

	/* header/version check */
	fprintf(out,
		"#pragma once\n"
		"\n"
		"// auto-generated, do not modify\n"
	);
	fprintf(out, "static_assert(0x%08xU == Demo::Material::Version, \"Material definition mismatch, please recompile the maps\");\n", Demo::Material::Version);
	fprintf(out, "static_assert(0x%08xU == Demo::Entity::Version, \"Entity definition mismatch, please recompile the maps\");\n", Demo::Entity::Version);

	std::vector<char> source;
	for (auto& map_entry : DemoMaps) {
		printf("\n--- Compiling %s ---\n\n", map_entry.map_name);
		printf("Reading %s\n", map_entry.src_path);

		if (!ReadFile(map_entry.src_path, source))
			return 1;

		Map map;
		printf("Parsing map (%zd bytes)\n", source.size());
		if (!map.Parse({source.data(), source.size()}))
			return 1;

		if (!CompileMap(map, map_entry.map_name, map_entry.src_file_name, options, out))
			return 1;
	}

	fflush(out);

	return 0;
}
 