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
				 (verts[j].pos[0] >> ChopBits) / float(MD3::Vertex::Scale),
				 (verts[j].pos[2] >> ChopBits) / float(MD3::Vertex::Scale),
				-(verts[j].pos[1] >> ChopBits) / float(MD3::Vertex::Scale)
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

void SortVertices(MD3::Header& model, const Options& options) {
	printf(INDENT "Sorting vertices\n");

	std::vector<u32> morton, new_to_old, old_to_new;
	std::vector<MD3::Vertex> sorted_verts;
	std::vector<MD3::UV> sorted_uvs;

	MD3::Surface* surf = model.GetFirstSurface();
	for (u32 i = 0; i < model.num_surfaces; ++i, surf = surf->GetNext()) {
		i16 mins[2] = {0x7fff, 0x7fff};
		i16 maxs[2] = {0x8000, 0x8000};

		auto* verts = surf->GetVerts();
		auto* uvs = surf->GetUVs();
		for (u16 j = 0; j < surf->num_verts; ++j) {
			auto& v = uvs[j];
			for (u16 k = 0; k < 2; ++k) {
				Math::assign_min(mins[k], v[k]);
				Math::assign_max(maxs[k], v[k]);
			}
		}

		for (u16 j = 0; j < 2; ++j)
			maxs[j] -= mins[j];

		morton.resize(surf->num_verts);
		new_to_old.resize(surf->num_verts);
		old_to_new.resize(surf->num_verts);
		sorted_verts.resize(surf->num_verts);
		sorted_uvs.resize(surf->num_verts);
		for (u16 j = 0; j < surf->num_verts; ++j)
			new_to_old[j] = j;
		
		for (u16 j = 0; j < surf->num_verts; ++j) {
			auto& v = uvs[j];
			i32 x = std::clamp(i32((v[0] - mins[0]) * 65535.f / maxs[0] + 0.5f), 0, 65535);
			i32 y = std::clamp(i32((v[1] - mins[1]) * 65535.f / maxs[1] + 0.5f), 0, 65535);
			morton[j] = Interleave2(x, y);
		}

		std::sort(new_to_old.begin(), new_to_old.end(), [&] (u32 a, u32 b) {
			return morton[a] < morton[b];
		});

		for (u16 j = 0; j < surf->num_verts; ++j) {
			auto old_vert = new_to_old[j];
			sorted_verts[j] = verts[old_vert];
			sorted_uvs[j] = uvs[old_vert];
			old_to_new[old_vert] = j;
		}
		memcpy(verts, sorted_verts.data(), surf->num_verts * sizeof(verts[0]));
		memcpy(uvs, sorted_uvs.data(), surf->num_verts * sizeof(uvs[0]));

		auto* tris = surf->GetTris();
		for (u16 j = 0; j < surf->num_tris; ++j) {
			tris[j].indices[0] = old_to_new[tris[j].indices[0]];
			tris[j].indices[1] = old_to_new[tris[j].indices[1]];
			tris[j].indices[2] = old_to_new[tris[j].indices[2]];
		}
	}
}

void SortIndices(MD3::Header& model, const Options& options) {
	printf(INDENT "Sorting indices\n");

	MD3::Surface* surf = model.GetFirstSurface();
	for (u32 i = 0; i < model.num_surfaces; ++i, surf = surf->GetNext()) {
		MD3::Triangle* tris = surf->GetTris();

		for (u16 j = 0; j < surf->num_tris; ++j) {
			auto& idx = tris[j].indices;
			u16 min_pos = 0;
			if (idx[1] < idx[0])
				min_pos = 1;
			if (idx[2] < idx[min_pos])
				min_pos = 2;
			std::rotate(idx, idx + min_pos, idx + 3);
		}

		std::sort(tris, tris + surf->num_tris, [&] (const MD3::Triangle& a, const MD3::Triangle& b) {
			return a.indices[0] < b.indices[0];
		});
	}
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

	const MD3::Surface*		surf	= model.GetFirstSurface();
	const MD3::Shader*		shaders	= surf->GetShaders();
	const MD3::Vertex*		verts	= surf->GetVerts();
	const MD3::UV*			uvs		= surf->GetUVs();

	auto quantize_pos = [](i16 value) {
		const u16
			ChopBits = 4,
			RoundingBias = (1 << ChopBits) - 1;
		bool negative = value < 0;
		value = (abs(value) + RoundingBias) >> ChopBits;
		return negative ? -value : value;
	};

	auto quantize_uv = [](float value) {
		return i16(std::floor(value * 128.f + 0.5f));
	};

	print << "const i16 "sv << "vertices"sv << "[] = {"sv;
	for (u16 pass = 0; pass < 5; ++pass) {
		i32 prev = 0;
		i32 current = 0;
		for (u32 i = 0; i < surf->num_verts; ++i) {
			switch (pass) {
				case 0:
				case 1:
				case 2:
					current = quantize_pos(verts[i].pos[pass]);
					print << (i32)EncodeSignMagnitude(current) << ","sv;
					prev = current;
					break;
				case 3:
				case 4:
					current = quantize_uv(uvs[i][pass - 3]);
					print << current << ","sv;
					prev = current;
					break;

				default:
					break;
			}
		}
	}
	print << "};"sv;
	print.Flush();

	const u32* indices = surf->GetIndices();
	print << "const u16 "sv << "indices"sv << "[] = {"sv;
	for (u16 i = 0; i < surf[0].num_tris * 3; ++i) {
		i32 idx = indices[i];
		if (i > 0)
			idx -= indices[i - 1];
		print << (i32)EncodeSignMagnitude(idx) << ","sv;
	}
	print << "};"sv;
	print.Flush();

	printf(INDENT "Wrote %d verts, %d tris\n", surf[0].num_verts, surf[0].num_tris);
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

		SortVertices(model, options);
		SortIndices(model, options);
		//ExportObj(model, (std::string("d:\\temp\\") + std::string(ExtractFileName({path})) + ".obj").c_str());
		CompileModel(model, options, path, out);
	}

	return num_errors;
}
