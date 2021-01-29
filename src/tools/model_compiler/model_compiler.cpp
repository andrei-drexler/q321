#include "../common.h"
#include "../../demo/resource_def.h"
#include "../../demo/material.h"
#include "mesh.h"
#include "forsyth.h"

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

/* rotate triangle indices so that the lowest index comes first */
void PrepareTriangleIndices(Mesh& mesh) {
	for (Mesh::Part& part : mesh.parts) {
		for (size_t i = 0; i < part.indices.size(); i += 3) {
			u32* idx = part.indices.data() + i;
			size_t min_pos = 0;
			if (idx[1] < idx[0])
				min_pos = 1;
			if (idx[2] < idx[min_pos])
				min_pos = 2;
			std::rotate(idx, idx + min_pos, idx + 3);
		}
	}
}

////////////////////////////////////////////////////////////////

void CompileModel(const Mesh& mesh, const Options& options, const std::string& path, FILE* out) {
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

	std::vector<u16> output_vertices;
	std::vector<u16> output_indices;
	std::vector<Part> output_parts;

	for (const Mesh::Part& part : mesh.parts) {
		output_parts.push_back({(u32)part.material, (u32)part.vertices.size(), (u32)part.indices.size()});

		/* write part vertices */
		Vertex previous = {};
		for (u16 i = 0; i < part.vertices.size(); ++i) {
				Vertex current;
				const Mesh::Vertex& src = part.vertices[i];
				current.pos[0] = quantize_pos(src.pos[0]);
				current.pos[1] = quantize_pos(src.pos[1]);
				current.pos[2] = quantize_pos(src.pos[2]);
				current.uv[0] = quantize_uv(src.uv[0]);
				current.uv[1] = quantize_uv(src.uv[1]);

				if (DEMO_MODELS_USE_DELTA_ENCODING) {
					Vertex residual;
					for (u32 field = 0; field < std::size(current.data); ++field)
						residual.data[field] = current.data[field] - previous.data[field];
					previous = current;

					output_vertices.push_back(EncodeSignMagnitude(residual.pos[0]));
					output_vertices.push_back(EncodeSignMagnitude(residual.pos[1]));
					output_vertices.push_back(EncodeSignMagnitude(residual.pos[2]));
					output_vertices.push_back(EncodeSignMagnitude(residual.uv[0]));
					output_vertices.push_back(EncodeSignMagnitude(residual.uv[1]));
				} else {
					output_vertices.push_back(EncodeSignMagnitude(current.pos[0]));
					output_vertices.push_back(EncodeSignMagnitude(current.pos[1]));
					output_vertices.push_back(EncodeSignMagnitude(current.pos[2]));
					output_vertices.push_back(current.uv[0]);
					output_vertices.push_back(current.uv[1]);
				}
		}

		/* write part indices, pairing triangles if possible */
		u32 watermark = 0;
		for (size_t i = 0; i < part.indices.size(); i += 3) {
			const u32* tri = part.indices.data() + i;
			const u32* next = tri + 3;

			int edge_tri = -1; // edge starting position in current triangle [0..2] or -1
			int edge_next = -1; // (mirrored) edge starting position in next triangle [0..2] or -1
			if (i + 5 < part.indices.size()) { // we have at least 2 triangles left
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
				assert(idx < part.vertices.size());
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

	printf(INDENT "%zd parts, %zd verts, %zd tris\n",
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

	options.src_path = "../../../data/";
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

		Mesh mesh;
		ConvertModel(model, mesh);
		WeldVertices(mesh);
		Optimize(mesh);
		PrepareTriangleIndices(mesh);

		CompileModel(mesh, options, full_path, out);

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
