#include "../common.h"
#include "../../demo/resource_def.h"
#include "../../demo/material.h"
#include "q3model.h"

////////////////////////////////////////////////////////////////

#define INDENT "    "

struct Options {
	std::string				src_path;
	std::string				out_path;
};

////////////////////////////////////////////////////////////////

bool ExportObj(const MD3::Header& model, const char* path, float scale = 1.f, int frac_bits = 2) {
	FILE* out = fopen(path, "w");
	if (!out)
		return false;
	auto close_output = scope_exit { fclose(out); out = NULL; };

	const i16
		ChopBits = std::max(MD3::Vertex::FracBits - frac_bits, 0),
		RoundBias = ChopBits > 0 ? 1 << (ChopBits - 1) : 0,
		RoundMask = (1 << ChopBits) - 1
	;
	const float VertexScale = scale / MD3::Vertex::Scale;

	std::vector<vec3> normals;

	const MD3::Surface* surf = model.GetFirstSurface();
	for (u32 i = 0, base = 1; i < model.num_surfaces; ++i, base += surf->num_verts, surf = surf->GetNext()) {
		//fprintf(out, "g surface_%d\n", i);
		fprintf(out, "g %s\n", surf->GetShaders()[0].name);

		auto verts = MD3::GetVertices(*surf);
		auto tris = MD3::GetTriangles(*surf);
		auto uvs = MD3::GetUVs(*surf);

		// Note: we don't store the original normals in the compiled exe,
		// so we also recompute them here to replicate demo behavior.

		normals.clear();
		normals.resize(surf->num_verts, {0.f, 0.f, 0.f});

		for (const MD3::Triangle& tri : tris) {
			vec3 pos[3];
			pos[0] = verts[tri.indices[0]].GetPosition();
			pos[1] = verts[tri.indices[1]].GetPosition();
			pos[2] = verts[tri.indices[2]].GetPosition();

			vec3 delta[3];
			delta[0] = pos[1] - pos[0];
			delta[1] = pos[2] - pos[1];
			delta[2] = pos[0] - pos[2];

			for (u32 i = 0; i < 3; ++i)
				normals[tri.indices[i]] += cross(delta[i], delta[(i+1) % 3]);
		}

		for (vec3& normal : normals)
			safe_normalize(normal);

		for (u32 j = 0; j < surf->num_verts; ++j) {
			fprintf(out,
				"v %g %g %g\n"
				"vt %g %g\n"
				"vn %g %g %g\n",
				 ((verts[j].pos[0] + RoundBias) >> ChopBits << ChopBits) * VertexScale,
				 ((verts[j].pos[2] + RoundBias) >> ChopBits << ChopBits) * VertexScale,
				-((verts[j].pos[1] + RoundBias) >> ChopBits << ChopBits) * VertexScale,
				uvs[j][0],
				1.f - uvs[j][1],
				-normals[j][0],
				-normals[j][2],
				 normals[j][1]
			);
		}

		for (u32 j = 0; j < surf->num_tris; ++j) {
			fprintf(out, "f %d/%d/%d %d/%d/%d %d/%d/%d\n",
				tris[j].indices[0] + base,
				tris[j].indices[0] + base,
				tris[j].indices[0] + base,
				tris[j].indices[2] + base,
				tris[j].indices[2] + base,
				tris[j].indices[2] + base,
				tris[j].indices[1] + base,
				tris[j].indices[1] + base,
				tris[j].indices[1] + base
			);
		}
	}
	
	return true;
}

////////////////////////////////////////////////////////////////

void AdaptiveQuantization(MD3::Header& model, float quality = 2.f) {
	MD3::VertexPositionHashMap<u16> min_vertex_dist;

	/* find per-vertex minimum edge distances */
	for (MD3::Surface& surf : MD3::GetSurfaces(model)) {
		MD3::Triangle* tris = surf.GetTris();
		MD3::Vertex* verts = surf.GetVerts();

		for (u16 i = 0; i < surf.num_verts; ++i) {
			min_vertex_dist[verts[i]] = 0x7fff;
		}

		for (u16 i = 0; i < surf.num_tris; ++i) {
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = tris[i].indices[j];
				u32 i1 = tris[i].indices[(j + 1) % 3];
				MD3::Vertex& v0 = verts[i0];
				MD3::Vertex& v1 = verts[i1];
				u16& min_dist = min_vertex_dist[v0];
				u16 edge_dist = 0;
				for (u16 k = 0; k < 3; ++k)
					edge_dist = std::max(edge_dist, (u16)abs(v1.pos[k] - v0.pos[k])); // l1 norm
				min_dist = std::min(min_dist, edge_dist);
			}
		}
	}

	/* propagate min dist to neighbors */
	for (MD3::Surface& surf : MD3::GetSurfaces(model)) {
		for (MD3::Triangle& tri : MD3::GetTriangles(surf)) {
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = tri.indices[j];
				u32 i1 = tri.indices[(j + 1) % 3];
				MD3::Vertex& v0 = surf.GetVerts()[i0];
				MD3::Vertex& v1 = surf.GetVerts()[i1];
				u16& d0 = min_vertex_dist[v0];
				u16& d1 = min_vertex_dist[v1];
				d0 = std::min(d0, d1);
			}
		}
	}

	/* snap vertices */
	for (MD3::Surface& surf : MD3::GetSurfaces(model)) {
		for (MD3::Vertex& v : MD3::GetVertices(surf)) {
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

////////////////////////////////////////////////////////////////

void WeldVertices(MD3::Surface& surf) {
	struct VertexRefHasher {
		MD3::Surface* surface;

		/* equality comparison */
		bool operator()(u32 i0, u32 i1) const {
			const MD3::Vertex* v0 = surface->GetVerts() + i0;
			const MD3::Vertex* v1 = surface->GetVerts() + i1;
			const MD3::UV* uv0 = surface->GetUVs() + i0;
			const MD3::UV* uv1 = surface->GetUVs() + i1;
			return
				0 == memcmp(v0,  v1,  sizeof(*v0)) &&
				0 == memcmp(uv0, uv1, sizeof(*uv0))
			;
		}

		/* hashing */
		size_t operator()(u32 index) const {
			const MD3::Vertex& v = surface->GetVerts()[index];
			const MD3::UV& uv = surface->GetUVs()[index];
			size_t result = HashValue(v.pos[0]);
			result = HashCombine(result, v.pos[1]);
			result = HashCombine(result, v.pos[2]);
			result = HashCombine(result, v.nor);
			result = HashCombine(result, uv[0]);
			result = HashCombine(result, uv[1]);
			return result;
		}
	};

	using VertexHashMap = std::unordered_map<u32, u32, VertexRefHasher, VertexRefHasher>;
	
	/* initializate hasher and key_eq with surface pointer */
	VertexHashMap hash_map(surf.num_verts * 2, {&surf}, {&surf});

	std::vector<u32> remap(surf.num_verts);

	u32 removed = 0;
	for (u32 i = 0; i < surf.num_verts; ++i) {
		u32& index = hash_map[i];
		if (index == 0)
			index = i + 1;
		remap[i] = index - 1;
		if (i != index - 1)
			++removed;
	}

	for (u32& i : MD3::GetIndices(surf))
		i = remap[i];

	if (0) {
		printf(INDENT"%s: %d/%d vertices removed after welding\n",
			surf.GetShaders()[0].name, removed, surf.num_verts
		);
	}
}

////////////////////////////////////////////////////////////////

namespace Forsyth {
	/*
	Linear-Speed Vertex Cache Optimisation / Tom Forsyth
	https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html
	*/

	static constexpr u32
		CacheSize = 32;

	static constexpr float
		FindVertexScore_CacheDecayPower = 1.5f,
		FindVertexScore_LastTriScore = 2.5f, // changed from 0.75f to improve compression
		FindVertexScore_ValenceBoostScale = 2.0f,
		FindVertexScore_ValenceBoostPower = 0.5f;

	struct VertexData {
		u32		num_tris = 0;
		u32		num_active_tris = 0;
		u32*	tris = nullptr;
		i8		cache_pos = -1;
		float	score = -1.f;

		void ComputeScore() {
			if (num_active_tris == 0) {
				// no tri needs this vertex
				score = -1.f;
				return;
			}

			score = 0.f;
			if (cache_pos >= 0) {
				if (cache_pos < 3) {
					// This vertex was used in the last triangle,
					// so it has a fixed score, whichever of the three
					// it's in. Otherwise, you can get very different
					// answers depending on whether you add
					// the triangle 1,2,3 or 3,1,2 - which is silly.
					score = FindVertexScore_LastTriScore;
				} else {
					assert(cache_pos < CacheSize);
					// Points for being high in the cache.
					const float Scaler = 1.f / (CacheSize - 3);
					score = 1.f - float(cache_pos - 3) * Scaler;
					score *= sqrtf(score); // score = pow(score, FindVertexScore_CacheDecayPower);
				}
			}

			// Bonus points for having a low number of tris still to
			// use the vert, so we get rid of lone verts quickly.

			// float valence_boost = powf(vertex.num_active_tris, -FindVertexScore_ValenceBoostPower);
			float valence_boost = 1.f / sqrtf((float)num_active_tris);
			score += FindVertexScore_ValenceBoostScale * valence_boost;
		}
	};

	size_t ReorderIndices(u32* indices, size_t num_indices, size_t num_vertices) {
		size_t num_initial_tris = num_indices / 3; // includes degenerate triangles
		size_t num_tris = 0;

		/* count non-degenerate triangles */
		for (size_t i = 0; i < num_initial_tris; ++i) {
			const u32* idx = indices + i * 3;
			if (idx[0] != idx[1] && idx[0] != idx[2] && idx[1] != idx[2]) {
				indices[num_tris * 3 + 0] = idx[0];
				indices[num_tris * 3 + 1] = idx[1];
				indices[num_tris * 3 + 2] = idx[2];
				++num_tris;
			}
		}
		
		/* zero out trailing degenerate triangles */
		memset(indices + num_tris * 3, 0, (num_indices - num_tris * 3) * sizeof(indices[0]));
		num_indices = num_tris * 3;

		std::vector<VertexData>		vertex_data(num_vertices);
		std::vector<u32>			vertex_tris(num_indices);
		std::vector<bool>			is_tri_added(num_tris);
		std::vector<float>			tri_score(num_tris);

		for (size_t i = 0; i < num_indices; ++i)
			++vertex_data[indices[i]].num_tris;
		
		/* counts to offsets */
		size_t offset = 0;
		for (size_t i = 0; i < num_vertices; ++i) {
			VertexData& v = vertex_data[i];
			v.tris = vertex_tris.data() + offset;
			offset += v.num_tris;
			v.num_active_tris = v.num_tris;
			v.num_tris = 0;
		}
		
		for (size_t i = 0; i < num_indices; ++i) {
			u32 idx = indices[i];
			VertexData& v = vertex_data[idx];
			v.tris[v.num_tris++] = i / 3;
		}

		for (auto& v : vertex_data) {
			assert(v.num_active_tris == v.num_tris);
			v.ComputeScore();
		}

		for (size_t i = 0; i < num_tris; ++i) {
			const u32* idx = indices + i * 3;
			tri_score[i] =
				vertex_data[idx[0]].score +
				vertex_data[idx[1]].score +
				vertex_data[idx[2]].score
			;
		}

		i32 cache[CacheSize + 3];
		for (auto& v : cache)
			v = -1;

		auto assert_cache_integrity = [&] {
			for (size_t i = 0; i < CacheSize; ++i) {
				if (cache[i] != -1) {
					assert(cache[i] >= 0);
					assert(cache[i] < num_vertices);
					assert(vertex_data[cache[i]].cache_pos == i);
				}
			}
		};

		i32 best_triangle = -1;
		size_t num_tris_added = 0;

		std::vector<u32> output(num_indices);

		while (num_tris_added < num_tris) {
			if (best_triangle == -1) {
				float best_tri_score = -FLT_MAX;
				for (size_t i = 0; i < num_tris; ++i) {
					if (!is_tri_added[i] && tri_score[i] > best_tri_score) {
						best_tri_score = tri_score[i];
						best_triangle = i;
					}
				}
			}
			
			assert(best_triangle != -1);
			if (best_triangle == -1)
				break;

			const u32* idx = indices + best_triangle * 3;
			output[num_tris_added * 3 + 0] = idx[0];
			output[num_tris_added * 3 + 1] = idx[1];
			output[num_tris_added * 3 + 2] = idx[2];

			is_tri_added[best_triangle] = true;
			++num_tris_added;

			size_t write_pos = 0;
			for (size_t i = 0; i < CacheSize; ++i) {
				i32 entry = cache[i];
				if (entry == -1)
					break;
				if (entry != idx[0] && entry != idx[1] && entry != idx[2])
					cache[write_pos++] = entry;
			}
			if (write_pos > 0) {
				cache[write_pos] = -1;
				memmove(cache + 3, cache, write_pos * sizeof(cache[0]));
			}
			cache[0] = idx[0];
			cache[1] = idx[1];
			cache[2] = idx[2];

			for (size_t i = 3; i < CacheSize + 3; ++i) {
				assert(cache[i] != idx[0]);
				assert(cache[i] != idx[1]);
				assert(cache[i] != idx[2]);
			}

			/* remove current triangle from the active list of its 3 vertices */
			for (size_t i = 0; i < 3; ++i) {
				VertexData& v = vertex_data[idx[i]];
				size_t pos;
				for (pos = 0; pos < v.num_active_tris; ++pos) {
					if (v.tris[pos] == best_triangle) {
						break;
					}
				}
				assert(pos != v.num_active_tris);
				if (pos + 1 < v.num_active_tris)
					v.tris[pos] = v.tris[v.num_active_tris - 1];
				--v.num_active_tris;
			}

			best_triangle = -1;
			float best_score = -FLT_MAX;

			for (size_t i = 0; i < CacheSize + 3; ++i) {
				i32& index = cache[i];
				if (index == -1)
					break;

				VertexData& v = vertex_data[index];

				if (i >= CacheSize) {
					/* evicted vertex */
					assert(index != idx[0]);
					assert(index != idx[1]);
					assert(index != idx[2]);
					v.cache_pos = -1;
					index = -1;
				} else {
					v.cache_pos = i8(i);
				}

				/* update scores of active triangles */
				float old_score = v.score;
				v.ComputeScore();
				float delta = v.score - old_score;

				for (u32 j = 0; j < v.num_active_tris; ++j) {
					i32 triangle = v.tris[j];
					assert(!is_tri_added[triangle]);
					float& score = tri_score[triangle];
					score += delta;
					if (score > best_score) {
						best_score = score;
						best_triangle = triangle;
					}
				}
			}

			//assert_cache_integrity();
		}

		memcpy(indices, output.data(), num_indices * sizeof(u32));

		return num_tris * 3;
	}
}

////////////////////////////////////////////////////////////////

int FindEdge(const u32* indices, u32 a, u32 b) {
	for (int i = 0; i < 3; ++i)
		if (indices[i] == a && indices[(i + 1) % 3] == b)
			return i;
	return -1;
}

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

void CompileModel(MD3::Header& model, const Options& options, const std::string& path, FILE* out) {
	string_view clean_path = path;
	auto name = ExtractFileName({path});
	while (RemovePrefix(clean_path, "../"sv))
		;

	fprintf(out, "\n// %.*s\n", int(clean_path.size()), clean_path.data());
	fprintf(out, "namespace %.*s {\n", int(name.size()), name.data());
	auto write_footer = scope_exit {
		fprintf(out, "} // namespace %.*s\n", int(name.size()), name.data());
	};

	ArrayPrinter print(out);

	auto quantize_pos = [](i16 value) {
		const u16
			ChopBits = 4,
			RoundingBias = 1 << (ChopBits - 1);
		return (value + RoundingBias) >> ChopBits;
	};

	auto quantize_uv = [](float value) {
		return i16(std::floor(value * 64.f + 0.5f));
	};

	struct Part {
		u32 material;
		u32 num_vertices;
		u32 num_indices;
	};

	union Vertex {
		struct {
			i16 pos[3];
			i16 uv[2];
		};
		i16 data[5];
	};

	std::vector<u32> flipped_indices;
	std::vector<u16> output_vertices;
	std::vector<u16> output_indices;
	std::vector<Part> output_parts;
	std::vector<i32> old_to_new;
	std::vector<Vertex> sorted_vertices;

	MD3::Surface* surf = model.GetFirstSurface();
	for (u32 surface_index = 0; surface_index < model.num_surfaces; ++surface_index, surf = surf->GetNext()) {
		const MD3::Shader*		shaders	= surf->GetShaders();
		const MD3::Vertex*		verts	= surf->GetVerts();
		const MD3::UV*			uvs		= surf->GetUVs();

		const u32* base_indices = surf->GetIndices();

		auto& name = surf->GetShaders()[0].name;
		for (size_t i = 0; name[i]; ++i)
			name[i] = std::tolower(name[i]);

		assert(surf->num_shaders > 0);
		u32 material = FindMaterial(name);
		u32 props = Demo::Material::Properties[material];
		
		/* skip invisible surfaces */
		if ((props & Demo::Material::MaskVisibility) == Demo::Material::Invisible)
			continue;

		if (!(props & Demo::Material::NeedsUV))
			memset(surf->GetUVs(), 0, surf->num_verts * sizeof(uvs[0]));
		WeldVertices(*surf);

		flipped_indices.clear();
		flipped_indices.resize(surf->num_tris * 3);
		/* flip faces, the lazy way */
		for (size_t i = 0; i < flipped_indices.size(); i += 3) {
			flipped_indices[i + 0] = base_indices[i + 0];
			flipped_indices[i + 1] = base_indices[i + 2];
			flipped_indices[i + 2] = base_indices[i + 1];
		}

		size_t num_indices = Forsyth::ReorderIndices(flipped_indices.data(), flipped_indices.size(), surf->num_verts);
		flipped_indices.resize(num_indices);

		/* reindex vertices */
		old_to_new.clear();
		old_to_new.resize(surf->num_verts, -1);
		
		u32 num_vertices = 0;
		for (auto& index : flipped_indices) {
			i32& new_index = old_to_new[index];
			if (new_index == -1)
				new_index = num_vertices++;
			index = new_index;
		}

		/* rotate triangle indices so that the lowest index comes first */
		for (u16 i = 0; i < num_indices; i += 3) {
			u32* idx = flipped_indices.data() + i;
			u16 min_pos = 0;
			if (idx[1] < idx[0])
				min_pos = 1;
			if (idx[2] < idx[min_pos])
				min_pos = 2;
			std::rotate(idx, idx + min_pos, idx + 3);
		}

		output_parts.push_back({material, num_vertices, num_indices});

		/* sort vertices */
		sorted_vertices.clear();
		sorted_vertices.resize(num_vertices);

		for (size_t src = 0; src < surf->num_verts; ++src) {
			i32 new_index = old_to_new[src];
			if (new_index == -1)
				continue;
			Vertex& dst = sorted_vertices[new_index];
			dst.pos[0] = quantize_pos(verts[src].pos[0]);
			dst.pos[1] = quantize_pos(verts[src].pos[1]);
			dst.pos[2] = quantize_pos(verts[src].pos[2]);
			dst.uv[0] = quantize_uv(uvs[src][0]);
			dst.uv[1] = quantize_uv(uvs[src][1]);
		}

		/* enforce X axis sprite orientation */
		if (props & Demo::Material::Sprite) {
			for (Vertex& v : sorted_vertices) {
				if (!v.pos[1]) {
					v.pos[1] = v.pos[0];
					v.pos[0] = 0;
				}
			}
		}

		/* write part vertices */
		for (u16 i = 0; i < num_vertices; ++i) {
			if (DEMO_MODELS_USE_DELTA_ENCODING) {
				Vertex prediction = {};
				if (i > 0)
					prediction = sorted_vertices[i - 1];

				Vertex residual;
				for (u32 field = 0; field < std::size(prediction.data); ++field)
					residual.data[field] = sorted_vertices[i].data[field] - prediction.data[field];

				output_vertices.push_back(EncodeSignMagnitude(residual.pos[0]));
				output_vertices.push_back(EncodeSignMagnitude(residual.pos[1]));
				output_vertices.push_back(EncodeSignMagnitude(residual.pos[2]));
				output_vertices.push_back(EncodeSignMagnitude(residual.uv[0]));
				output_vertices.push_back(EncodeSignMagnitude(residual.uv[1]));
			} else {
				output_vertices.push_back(EncodeSignMagnitude(sorted_vertices[i].pos[0]));
				output_vertices.push_back(EncodeSignMagnitude(sorted_vertices[i].pos[1]));
				output_vertices.push_back(EncodeSignMagnitude(sorted_vertices[i].pos[2]));
				output_vertices.push_back(sorted_vertices[i].uv[0]);
				output_vertices.push_back(sorted_vertices[i].uv[1]);
			}
		}

		/* write part indices, pairing triangles if possible */
		u32 watermark = 0;
		for (size_t i = 0; i < num_indices; i += 3) {
			const u32* tri = flipped_indices.data() + i;
			const u32* next = tri + 3;

			int edge_tri = -1; // edge starting position in current triangle [0..2] or -1
			int edge_next = -1; // (mirrored) edge starting position in next triangle [0..2] or -1
			if (i + 5 < num_indices) { // we have at least 2 triangles left
				for (size_t j = 0; j < 3; ++j) {
					u32 v0 = tri[j];
					u32 v1 = tri[(j + 1) % 3];
					edge_next = FindEdge(next, v1, v0);
					if (edge_next != -1) {
						edge_tri = j;
						break;
					}
				}
			}

			u32 buffer[4];
			u32 num_entries = 0;

			if (edge_next != -1) { // sharing an edge with the next triangle?
				u32 edge_v0 = tri[edge_tri];
				u32 edge_v1 = tri[(edge_tri + 1) % 3];

				// For paired triangles, the first 2 indices represent the shared edge.
				// Note: first edge index *MUST* be lower than the second, this is how
				// the decoder distinguishes paired triangles from single ones.

				if (edge_v0 < edge_v1) {
					// already in the right order
					buffer[0] = edge_v0;
					buffer[1] = edge_v1;
					buffer[2] = tri[(edge_tri + 2) % 3];
					buffer[3] = next[(edge_next + 2) % 3];
				} else {
					// If the indices are in the wrong order in the first triangle,
					// then they must be in the right order in the second triangle,
					// so we write the second triangle first.
					// Not: this affects the max watermark delta.
					buffer[0] = edge_v1;
					buffer[1] = edge_v0;
					buffer[2] = next[(edge_next + 2) % 3];
					buffer[3] = tri[(edge_tri + 2) % 3];
				}
				i += 3;
				num_entries = 4;
				assert(buffer[0] < buffer[1]);
			} else {
				buffer[0] = tri[2];
				buffer[1] = tri[0];
				buffer[2] = tri[1];
				num_entries = 3;
				assert(buffer[0] > buffer[1]);
			}

			for (u32 j = 0; j < num_entries; ++j) {
				u32 idx = buffer[j];
				assert(idx < num_vertices);
				output_indices.push_back(watermark + 3 - idx);
				watermark = std::max<u32>(watermark, idx);
			}
		}
	}

	size_t vertex_stream_lengths[5];
	std::vector<u8> output_varint_vertices;
	output_varint_vertices.reserve(output_vertices.size() * 2);
	size_t prev_size = 0;
	for (u8 pass = 0; pass < 5; ++pass) {
		for (size_t i = pass; i < output_vertices.size(); i += 5)
			WriteVarint(output_varint_vertices, output_vertices[i]);
		vertex_stream_lengths[pass] = output_varint_vertices.size() - prev_size;
		prev_size = output_varint_vertices.size();
	}

	print << "const u8 vertices[] = {"sv;
	for (auto v : output_varint_vertices)
		print << i32(v) << ","sv;
	print << "};"sv;
	print.Flush();

	print << "const u8 indices[] = {"sv;
	for (auto v : output_indices)
		WriteVarint(print, v);
	print << "};"sv;
	print.Flush();

	size_t num_vertices = 0;
	size_t num_tris = 0;
	print << "const Demo::PackedModel::Part parts[] = {"sv;
	for (auto& part : output_parts) {
		print
			<< "{"sv
			<<		(i32)part.material << ","sv
			<<		(i32)part.num_vertices << ","sv
			<<		(i32)part.num_indices << ","sv
			<< "},"sv
		;
		num_vertices += part.num_vertices;
		num_tris += part.num_indices / 3;
	}
	print << "};"sv;
	print.Flush();

	print << "const u16 stream_lengths[] = {"sv;
	for (auto v : vertex_stream_lengths)
		print << i32(v - num_vertices) << ","sv;
	print << "};"sv;
	print.Flush();

	printf(INDENT "%d parts, %d verts, %d tris\n",
		output_parts.size(), num_vertices, num_tris
	);
}

////////////////////////////////////////////////////////////////

static constexpr std::string_view ModelPaths[] = {
	#define PP_DEMO_MODEL_PATH(path,name)		#path "/" #name ".md3",
	DEMO_MODELS(PP_DEMO_MODEL_PATH)
	#undef PP_DEMO_MODEL_PATH
};

int main() {
	Options options;

	options.src_path = "../../demo/data/";
	options.out_path = "../../demo/cooked/cooked_models.h";

	FILE* out = fopen(options.out_path.c_str(), "w");
	if (!out)
		return false;
	auto close_output = scope_exit { fclose(out); out = NULL; };

	/* header/version check */
	fprintf(out,
		"#pragma once\n"
		"\n"
		"// auto-generated, do not modify\n"
		"static_assert(0x%08xU == Demo::Model::Version, \"Model definition mismatch, please recompile the model compiler & models\");\n",
		Demo::Model::Version
	);

	std::vector<char> contents;
	int num_errors = 0;

	std::vector<std::string> model_names;
	model_names.reserve(256);

	for (auto path : ModelPaths) {
		printf("Compiling %.*s\n", int(path.length()), path.data());

		std::string full_path = options.src_path + std::string{path};
		if (!ReadFile(full_path.c_str(), contents)) {
			++num_errors;
			continue;
		}

		auto& model = *(MD3::Header*)contents.data();
		if (!CheckModel(model)) {
			++num_errors;
			continue;
		}

		//AdaptiveQuantization(model);
		//ExportObj(model, (std::string("d:\\temp\\") + std::string(ExtractFileName({path})) + ".obj").c_str());
		CompileModel(model, options, full_path, out);

		model_names.emplace_back(ExtractFileName({path}));
	}

	/* footer */
	fprintf(out,
		"\n"
		"////////////////////////////////////////////////////////////////\n"
		"\n"
		"static constexpr Demo::PackedModel cooked_models[] = {\n"
		"\t#define PP_ADD_MODEL_ENTRY(path,name,...) {(u8)size(name::parts),name::parts,name::vertices,name::stream_lengths,name::indices},\n"
		"\tDEMO_MODELS(PP_ADD_MODEL_ENTRY)\n"
		"\t#undef PP_ADD_MODEL_ENTRY\n"
		"};\n"
	);

	return num_errors;
}
