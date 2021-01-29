#pragma once

//
// Linear-Speed Vertex Cache Optimisation / Tom Forsyth
// https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html
//

namespace Forsyth {
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
