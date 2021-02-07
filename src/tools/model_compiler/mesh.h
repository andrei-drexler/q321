#pragma once

#include "q3model.h"
#include "forsyth.h"

////////////////////////////////////////////////////////////////

struct Mesh {
	using Pos = i16[3];
	using UV = i16[2];

	struct Vertex {
		static const int PosScale = MD3::Vertex::FracBits;
		static const int UVScale = 4096;

		i16		pos[3];
		float	uv[2];
		i16		nor;
	};

	struct Part {
		i16 material;

		std::vector<Vertex> vertices;
		std::vector<u32> indices;
	};

	std::vector<Part> parts;
};

////////////////////////////////////////////////////////////////

static constexpr string_view MaterialSubstitutions[][2] = {
	#define PP_ADD_MATERIAL_SUBSTITUTION(src, dst) {src, dst},
	DEMO_MATERIAL_SUBSTITUTIONS(PP_ADD_MATERIAL_SUBSTITUTION)
	#undef PP_ADD_MATERIAL_SUBSTITUTION
};

u32 FindMaterial(string_view name) {
	RemovePrefix(name, "textures/");
	RemoveSuffix(name, ".tga");

	string_view fallback_path =
		StartsWith(name, "models/mapobjects/"sv) ? "*map_model"sv : "*item_model"sv;

	u32 fallback_material = 0;

	for (auto& sub : MaterialSubstitutions) {
		if (name == sub[0]) {
			name = sub[1];
			break;
		}
	}

	for (u32 i = 0; i < std::size(Demo::Material::Paths); ++i) {
		string_view current_path = Demo::Material::Paths[i];
		if (name == current_path)
			return i;
		if (current_path == fallback_path)
			fallback_material = i;
	}

	return fallback_material;
}

////////////////////////////////////////////////////////////////

void ConvertModel(const MD3::Header& model, Mesh& mesh) {
	mesh.parts.reserve(model.num_surfaces);
	for (const MD3::Surface& src : MD3::GetSurfaces(model)) {
		if (src.num_shaders <= 0)
			continue;

		const MD3::String& base_shader_name = src.GetShaders()[0].name;
		MD3::String shader_name = {};
		for (size_t i = 0; base_shader_name[i]; ++i)
			shader_name[i] = std::tolower(base_shader_name[i]);

		u32 material = FindMaterial(shader_name);
		u32 props = Demo::Material::Properties[material];

		/* skip invisible surfaces */
		if ((props & Demo::Material::MaskVisibility) == Demo::Material::Invisible)
			continue;

		/* try to find an existing part with the same material */
		Mesh::Part* dst = nullptr;
		for (Mesh::Part& part : mesh.parts) {
			if (part.material == material) {
				dst = &part;
				break;
			}
		}

		/* add new part if needed */
		if (!dst) {
			dst = &mesh.parts.emplace_back();
			dst->material = material;
		}

		/* copy vertex data */
		const MD3::Vertex* src_verts = src.GetVerts();
		const MD3::UV* src_uvs = src.GetUVs();

		u32 base_vertex = dst->vertices.size();
		dst->vertices.resize(src.num_verts + base_vertex);

		for (u32 i = 0; i < src.num_verts; ++i) {
			Mesh::Vertex& dst_vert = dst->vertices[base_vertex + i];
			dst_vert.pos[0] = src_verts[i].pos[0];
			dst_vert.pos[1] = src_verts[i].pos[1];
			dst_vert.pos[2] = src_verts[i].pos[2];
			dst_vert.nor = src_verts[i].nor;
			dst_vert.uv[0] = src_uvs[i][0];
			dst_vert.uv[1] = src_uvs[i][1];

			/* remove UVs if not needed */
			if (!(props & Demo::Material::NeedsUV)) {
				dst_vert.uv[0] = 0;
				dst_vert.uv[1] = 0;
			}

			/* enforce X axis sprite orientation */
			if (props & Demo::Material::Sprite) {
				if (!dst_vert.pos[1]) {
					dst_vert.pos[1] = dst_vert.pos[0];
					dst_vert.pos[0] = 0;
				}
			}
		}

		/* copy (and flip) index data */
		const u32* src_indices = src.GetIndices();

		u32 base_index = dst->indices.size();
		dst->indices.resize(src.num_tris * 3 + base_index);

		for (u32 i = 0; i < src.num_tris; ++i) {
			dst->indices[base_index + i * 3 + 0] = base_vertex + src_indices[i * 3 + 0];
			dst->indices[base_index + i * 3 + 1] = base_vertex + src_indices[i * 3 + 2]; // flipped!
			dst->indices[base_index + i * 3 + 2] = base_vertex + src_indices[i * 3 + 1]; // flipped!
		}
	}
}

////////////////////////////////////////////////////////////////

void WeldVertices(Mesh::Part& part) {
	struct VertexRefHasher {
		Mesh::Part* part;

		/* equality comparison */
		bool operator()(u32 i0, u32 i1) const {
			return 0 == memcmp(&part->vertices[i0], &part->vertices[i1], sizeof(Mesh::Vertex));
		}

		/* hashing */
		size_t operator()(u32 index) const {
			const Mesh::Vertex& v = part->vertices[index];
			size_t result = HashValue(v.pos[0]);
			result = HashCombine(result, v.pos[1]);
			result = HashCombine(result, v.pos[2]);
			result = HashCombine(result, v.nor);
			result = HashCombine(result, v.uv[0]);
			result = HashCombine(result, v.uv[1]);
			return result;
		}
	};

	using VertexHashMap = std::unordered_map<size_t, size_t, VertexRefHasher, VertexRefHasher>;
	
	/* initializate hasher and key_eq with part pointer */
	VertexHashMap hash_map(part.vertices.size() * 2, {&part}, {&part});

	std::vector<size_t> remap(part.vertices.size());

	size_t removed = 0;
	for (size_t i = 0; i < part.vertices.size(); ++i) {
		size_t& index = hash_map[i];
		if (index == 0)
			index = i + 1;
		remap[i] = index - 1;
		if (i != index - 1)
			++removed;
	}

	for (size_t& i : part.indices)
		i = remap[i];
}

void WeldVertices(Mesh& mesh) {
	for (Mesh::Part& part : mesh.parts)
		WeldVertices(part);
}

////////////////////////////////////////////////////////////////

int FindEdge(const u32* indices, u32 a, u32 b) {
	for (int i = 0; i < 3; ++i)
		if (indices[i] == a && indices[(i + 1) % 3] == b)
			return i;
	return -1;
}

////////////////////////////////////////////////////////////////

void Optimize(Mesh& mesh) {
	std::vector<i32> old_to_new;
	std::vector<Mesh::Vertex> reordered_vertices;

	for (Mesh::Part& part : mesh.parts) {
		/* optimize indices */
		size_t num_indices = Forsyth::ReorderIndices(part.indices.data(), part.indices.size(), part.vertices.size());
		part.indices.resize(num_indices);

		/* optimize vertices */
		old_to_new.clear();
		reordered_vertices.clear();
		old_to_new.resize(part.vertices.size(), -1);
		reordered_vertices.reserve(part.vertices.size());

		for (u32& old_index : part.indices) {
			i32& new_index = old_to_new[old_index];
			if (new_index == -1) {
				new_index = reordered_vertices.size();
				reordered_vertices.push_back(part.vertices[old_index]);
			}
			old_index = new_index;
		}

		part.vertices.swap(reordered_vertices);
	}
}

////////////////////////////////////////////////////////////////

void AdaptiveQuantization(Mesh& mesh, float quality = 2.f) {
	struct PosHasher {
		/* equality comparison */
		bool operator()(const Mesh::Vertex& v0, const Mesh::Vertex& v1) const {
			return 0 == memcmp(&v0.pos, &v1.pos, sizeof(v0.pos));
		}

		/* hashing */
		size_t operator()(const Mesh::Vertex& v) const {
			size_t result = HashValue(v.pos[0]);
			result = HashCombine(result, v.pos[1]);
			result = HashCombine(result, v.pos[2]);
			return result;
		}
	};

	using VertexHashMap = std::unordered_map<Mesh::Vertex, u16, PosHasher, PosHasher>;
	VertexHashMap min_vertex_dist;

	/* find per-vertex minimum edge distances */
	for (Mesh::Part& part : mesh.parts) {
		for (size_t i = 0; i < part.vertices.size(); ++i) {
			min_vertex_dist[part.vertices[i]] = 0x7fff;
		}

		for (size_t i = 0; i < part.indices.size(); i += 3) {
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = part.indices[i + j];
				u32 i1 = part.indices[i + (j + 1) % 3];
				Mesh::Vertex& v0 = part.vertices[i0];
				Mesh::Vertex& v1 = part.vertices[i1];
				u16& min_dist = min_vertex_dist[v0];
				u16 edge_dist = 0;
				for (u16 k = 0; k < 3; ++k)
					edge_dist = std::max(edge_dist, (u16)abs(v1.pos[k] - v0.pos[k])); // l1 norm
				min_dist = std::min(min_dist, edge_dist);
			}
		}
	}

	/* propagate min dist to neighbors */
	for (Mesh::Part& part : mesh.parts) {
		for (size_t i = 0; i < part.indices.size(); i += 3) {
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = part.indices[i + j];
				u32 i1 = part.indices[i + (j + 1) % 3];
				Mesh::Vertex& v0 = part.vertices[i0];
				Mesh::Vertex& v1 = part.vertices[i1];
				u16& d0 = min_vertex_dist[v0];
				u16& d1 = min_vertex_dist[v1];
				d0 = std::min(d0, d1);
			}
		}
	}

	/* snap vertices */
	for (Mesh::Part& part : mesh.parts) {
		for (Mesh::Vertex& v : part.vertices) {
			u16 min_dist = min_vertex_dist[v];
			if (min_dist <= 0)
				continue;
			int bits = (int)std::floor(std::log2(min_dist) - quality);
			if (bits <= 0)
				continue;
			u16 bias = 1 << (bits - 1);
			u16 mask = (1 << bits) - 1;
			for (auto& k : v.pos)
				k = (k + bias) & ~mask;
		}
	}
}
