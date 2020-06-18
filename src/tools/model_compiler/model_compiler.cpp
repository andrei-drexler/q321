#include "../common.h"
#include "../print_array.h"
#include "../../demo/resource_def.h"
#include "q3model.h"

////////////////////////////////////////////////////////////////

#define INDENT "    "

struct Options {
	std::vector<std::string>		md3_paths;
	std::string						out_path;
};

////////////////////////////////////////////////////////////////

bool ExportObj(const MD3::Header& model, const char* path) {
	FILE* out = fopen(path, "w");
	if (!out)
		return false;
	auto close_output = scope_exit { fclose(out); out = NULL; };

	const i16 ChopBits = 4;

	const MD3::Surface* surf = model.GetFirstSurface();
	for (u32 i = 0, base = 1; i < model.num_surfaces; ++i, base += surf->num_verts, surf = surf->GetNext()) {
		fprintf(out, "g surface_%d\n", i);

		auto* verts = surf->GetVerts();
		for (u32 j = 0; j < surf->num_verts; ++j) {
			fprintf(out, "v %g %g %g\n",
				 (verts[j].pos[0] >> ChopBits << ChopBits) / float(MD3::Vertex::Scale),
				 (verts[j].pos[2] >> ChopBits << ChopBits) / float(MD3::Vertex::Scale),
				-(verts[j].pos[1] >> ChopBits << ChopBits) / float(MD3::Vertex::Scale)
			);
		}

		auto* tris = surf->GetTris();
		for (u32 j = 0; j < surf->num_tris; ++j) {
			fprintf(out, "f %d %d %d\n",
				tris[j].indices[0] + base,
				tris[j].indices[2] + base,
				tris[j].indices[1] + base
			);
		}
	}
	
	return true;
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
		FindVertexScore_LastTriScore = 1.5f, // changed from 0.75f to improve compression
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
			float valence_boost = 1.f / sqrtf(num_active_tris);
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
					v.cache_pos = i;
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

void CompileModel(const MD3::Header& model, const Options& options, const std::string& path, FILE* out) {
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
		return i16(std::floor(value * 128.f + 0.5f));
	};

	struct Part {
		u32 shader;
		u32 num_vertices;
		u32 num_indices;
	};

	struct Vertex {
		i16 pos[3];
		i16 uv[2];
	};

	std::vector<u32> flipped_indices;
	std::vector<u16> output_vertices;
	std::vector<u16> output_indices;
	std::vector<Part> output_parts;
	std::vector<i32> old_to_new;
	std::vector<Vertex> sorted_vertices;

	const MD3::Surface* surf = model.GetFirstSurface();
	for (u32 surface_index = 0; surface_index < model.num_surfaces; ++surface_index, surf = surf->GetNext()) {
		const MD3::Shader*		shaders	= surf->GetShaders();
		const MD3::Vertex*		verts	= surf->GetVerts();
		const MD3::UV*			uvs		= surf->GetUVs();

		const u32* base_indices = surf->GetIndices();

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

		output_parts.push_back({0, num_vertices, num_indices});

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

		/* write part vertices */
		u16 accum[5];
		memset(&accum, 0, sizeof(accum));

		auto delta_encode = [] (u16 current, u16& prev) {
			i16 delta = current - prev;
			prev = current;
			return EncodeSignMagnitude(delta);
		};

		for (u16 i = 0; i < num_vertices; ++i) {
			output_vertices.push_back(delta_encode(sorted_vertices[i].pos[0], accum[0]));
			output_vertices.push_back(delta_encode(sorted_vertices[i].pos[1], accum[1]));
			output_vertices.push_back(delta_encode(sorted_vertices[i].pos[2], accum[2]));
			output_vertices.push_back(delta_encode(sorted_vertices[i].uv[0], accum[3]));
			output_vertices.push_back(delta_encode(sorted_vertices[i].uv[1], accum[4]));
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

	print << "const u16 vertices[] = {"sv;
	for (u8 pass = 0; pass < 5; ++pass) {
		size_t step = output_vertices.size() / 5;
		for (size_t i = pass; i < output_vertices.size(); i += 5)
			print << i32(output_vertices[i]) << ","sv;
	}
	print << "};"sv;
	print.Flush();

	print << "const u16 indices[] = {"sv;
	for (auto v : output_indices)
		print << i32(v) << ","sv;
	print << "};"sv;
	print.Flush();

	print << "const Demo::PackedModel::Part parts[] = {"sv;
	for (auto& part : output_parts) {
		u8 shader = 0;
		print
			<< "{"sv
			<<		(i32)part.shader << ","sv
			<<		(i32)part.num_vertices << ","sv
			<<		(i32)part.num_indices << ","sv
			<< "},"sv
		;
	}
	print << "};"sv;
	print.Flush();

	printf(INDENT "%d parts, %d verts, %d tris\n",
		output_parts.size(), output_vertices.size() / 5, output_indices.size() / 3
	);
}

////////////////////////////////////////////////////////////////

int main() {
	Options options;

	options.md3_paths = {
		#define PP_DEMO_MODEL_PATH(name)		"../../demo/data/models/" #name ".md3",
		DEMO_MODELS(PP_DEMO_MODEL_PATH)
		#undef PP_DEMO_MODEL_PATH
	};
	options.out_path = "../../demo/cooked/cooked_models.h";

	FILE* out = fopen(options.out_path.c_str(), "w");
	if (!out)
		return false;
	auto close_output = scope_exit { fclose(out); out = NULL; };

	/* header */
	fprintf(out,
		"#pragma once\n"
		"\n"
		"// auto-generated, do not modify\n"
	);

	std::vector<char> contents;
	int num_errors = 0;

	std::vector<std::string> model_names;
	model_names.reserve(256);

	for (auto& path : options.md3_paths) {
		printf("Compiling %s\n", path.c_str());

		if (!ReadFile(path.c_str(), contents)) {
			++num_errors;
			continue;
		}
		
		auto& model = *(MD3::Header*)contents.data();
		if (!CheckModel(model)) {
			++num_errors;
			continue;
		}

		//ExportObj(model, (std::string("d:\\temp\\") + std::string(ExtractFileName({path})) + ".obj").c_str());
		CompileModel(model, options, path, out);

		model_names.emplace_back(ExtractFileName({path}));
	}

	/* footer */
	fprintf(out,
		"\n"
		"////////////////////////////////////////////////////////////////\n"
		"\n"
		"static constexpr Demo::PackedModel cooked_models[] = {\n"
		"\t#define PP_ADD_MODEL_ENTRY(name,...) {size(name::parts),size(name::vertices)/5,name::parts,name::vertices,name::indices},\n"
		"\tDEMO_MODELS(PP_ADD_MODEL_ENTRY)\n"
		"\t#undef PP_ADD_MODEL_ENTRY\n"
		"};\n"
	);

	return num_errors;
}
