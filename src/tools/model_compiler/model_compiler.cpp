#include "../common.h"
#include "../print_array.h"

#include "../../demo/resource_def.h"

////////////////////////////////////////////////////////////////

#define INDENT "    "

struct Options {
	std::vector<std::string>		md3_paths;
	std::string						out_path;
};

////////////////////////////////////////////////////////////////

namespace MD3 {
	const u32 MAX_QPATH = 64;
	using String = char[MAX_QPATH];

	////////////////////////////////////////////////////////////////

	constexpr u32 FourCC(const char* s) {
		return
			u8(s[0]) |
			(u8(s[1]) << 8) |
			(u8(s[2]) << 16) |
			(u8(s[3]) << 24);
	}

	////////////////////////////////////////////////////////////////
	
	template <typename T, typename Base>
	const T* GetPtr(const Base* base, u32 ofs) {
		return (const T*)(ofs + (const u8*)base);
	}

	template <typename T, typename Base>
	T* GetPtr(Base* base, u32 ofs) {
		return (T*)(ofs + (u8*)base);
	}
	
	////////////////////////////////////////////////////////////////

	struct Frame {
		vec3				bounds[2];
		vec3				local_origin;
		float				radius;
		char				name[16];
	};

	struct Triangle {
		u32					indices[3];
	};

	using UV = vec2;

	struct Vertex {
		static const i16 Scale = 64;

		i16					pos[3];
		i16					nor;
	};

	struct Shader {
		String				name;
		u32					index;
	};

	struct Tag {
		String				name;
		vec3				origin;
		vec3				axis[3];
	};

	struct Surface {
		u32					ident;
		String				name;
		u32					flags;
		
		u32					num_frames;
		u32					num_shaders;
		u32					num_verts;
		
		u32					num_tris;
		u32					ofs_tris;

		u32					ofs_shaders;
		u32					ofs_uvs;
		u32					ofs_verts;
		
		u32					ofs_end;
		
		Triangle*			GetTris()				{ return GetPtr<Triangle>(this, ofs_tris); }
		const Triangle*		GetTris() const			{ return GetPtr<Triangle>(this, ofs_tris); }
		Shader*				GetShaders()			{ return GetPtr<Shader>(this, ofs_shaders); }
		const Shader*		GetShaders() const		{ return GetPtr<Shader>(this, ofs_shaders); }
		UV*					GetUVs()				{ return GetPtr<UV>(this, ofs_uvs); }
		const UV*			GetUVs() const			{ return GetPtr<UV>(this, ofs_uvs); }
		Vertex*				GetVerts()				{ return GetPtr<Vertex>(this, ofs_verts); }
		const Vertex*		GetVerts() const		{ return GetPtr<Vertex>(this, ofs_verts); }
		
		Surface*			GetNext()				{ return GetPtr<Surface>(this, ofs_end); }
		const Surface*		GetNext() const			{ return GetPtr<Surface>(this, ofs_end); }
	};

	struct Header {
		static const u32 ExpectedIdent = FourCC("IDP3");
		static const u32 ExpectedVersion = 15;

		u32					ident;
		u32					version;
		String				name;
		u32					flags;

		i32					num_frames;
		u32					num_tags;
		u32					num_surfaces;
		u32					num_skins;

		u32					ofs_frames;
		u32					ofs_tags;
		u32					ofs_surfaces;

		u32					ofs_end;

		Frame*				GetFrames()				{ return GetPtr<Frame>(this, ofs_frames); }
		const Frame*		GetFrames() const		{ return GetPtr<Frame>(this, ofs_frames); }
		Surface*			GetSurfaces()			{ return GetPtr<Surface>(this, ofs_surfaces); }
		const Surface*		GetSurfaces() const		{ return GetPtr<Surface>(this, ofs_surfaces); }
		Tag*				GetTags()				{ return GetPtr<Tag>(this, ofs_tags); }
		const Tag*			GetTags() const			{ return GetPtr<Tag>(this, ofs_tags); }
	};
}

////////////////////////////////////////////////////////////////

bool ExportObj(const MD3::Header& model, const char* path) {
	FILE* out = fopen(path, "w");
	if (!out)
		return false;
	auto close_output = scope_exit { fclose(out); out = NULL; };

	const i16 ChopBits = 4;

	auto* surf = model.GetSurfaces();
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

bool CheckModel(const MD3::Header& model) {
	if (model.ident != model.ExpectedIdent) {
		return false;
	}

	if (model.version != model.ExpectedVersion) {
		return false;
	}

	if (model.num_frames < 1) {
		return false;
	}

	if (model.num_surfaces < 1) {
		return false;
	}

	return true;
}

////////////////////////////////////////////////////////////////

void SortVertices(MD3::Header& model, const Options& options) {
	printf(INDENT "Sorting vertices\n");

	std::vector<u32> morton, new_to_old, old_to_new;
	std::vector<MD3::Vertex> sorted_verts;
	std::vector<MD3::UV> sorted_uvs;

	auto* surf = model.GetSurfaces();
	for (u32 i = 0; i < model.num_surfaces; ++i, surf = surf->GetNext()) {
		i16 mins[3] = {0x7fff, 0x7fff, 0x7fff};
		i16 maxs[3] = {0x8000, 0x8000, 0x8000};

		auto* verts = surf->GetVerts();
		for (u16 j = 0; j < surf->num_verts; ++j) {
			auto& v = verts[j];
			for (u16 k = 0; k < 3; ++k) {
				Math::assign_min(mins[k], v.pos[k]);
				Math::assign_max(maxs[k], v.pos[k]);
			}
		}

		for (u16 j = 0; j < 3; ++j)
			maxs[j] -= mins[j];

		morton.resize(surf->num_verts);
		new_to_old.resize(surf->num_verts);
		old_to_new.resize(surf->num_verts);
		sorted_verts.resize(surf->num_verts);
		sorted_uvs.resize(surf->num_verts);
		for (u16 j = 0; j < surf->num_verts; ++j)
			new_to_old[j] = j;
		
		for (u16 j = 0; j < surf->num_verts; ++j) {
			auto& v = verts[j];
			i32 x = std::clamp(i32((v.pos[0] - mins[0]) * 1023.f / maxs[0] + 0.5f), 0, 1023);
			i32 z = std::clamp(i32((v.pos[2] - mins[2]) * 1023.f / maxs[2] + 0.5f), 0, 1023);
			i32 y = std::clamp(i32(abs(v.pos[1] - mins[1] - maxs[1] / 2) * (2.f * 1023.f) / maxs[1] + 0.5f), 0, 1023);
			morton[j] = Interleave3(x, y, z);
		}

		std::sort(new_to_old.begin(), new_to_old.end(), [&] (u32 a, u32 b) {
			return morton[a] < morton[b];
		});

		auto* uvs = surf->GetUVs();
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

	auto* surf = model.GetSurfaces();
	for (u32 i = 0; i < model.num_surfaces; ++i, surf = surf->GetNext()) {
		auto* tris = surf->GetTris();
		
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
	constexpr string_view Prefix = "../";
	while (clean_path.size() >= Prefix.size() && 0 == memcmp(clean_path.data(), Prefix.data(), Prefix.size()))
		clean_path.remove_prefix(Prefix.size());

	fprintf(out, "\n// %.*s\n", int(clean_path.size()), clean_path.data());
	fprintf(out, "namespace %.*s {\n", int(name.size()), name.data());
	auto write_footer = scope_exit {
		fprintf(out, "} // namespace %.*s\n", int(name.size()), name.data());
	};

	ArrayPrinter print(out);

	auto* surf = model.GetSurfaces();
	auto* shaders = surf->GetShaders();
	auto* verts = surf->GetVerts();
	auto* uvs = surf->GetUVs();
	auto* tris = surf->GetTris();

	auto encode_pos = [](i16 value) {
		const u16 ChopBits = 4;
		bool negative = value < 0;
		value = (abs(value) + ((1 << ChopBits) - 1)) >> ChopBits;
		return u16((value << 1) + negative);
	};

	print << "const i16 "sv << "vertices"sv << "[] = {"sv;
	for (u16 pass = 0; pass < 3; ++pass) {
		for (u32 i = 0; i < surf->num_verts; ++i) {
			auto& v = verts[i];
			switch (pass) {
				case 0: print << encode_pos(v.pos[0]) << ","sv; break;
				case 1: print << encode_pos(v.pos[1]) << ","sv; break;
				case 2: print << encode_pos(v.pos[2]) << ","sv; break;
			}
		}
	}
	print << "};"sv;
	print.Flush();

	print << "const i16 "sv << "uvs"sv << "[] = {"sv;
	for (u16 pass = 0; pass < 2; ++pass) {
		for (u32 i = 0; i < surf[0].num_verts; ++i) {
			auto v = uvs[i][pass];
			print << i16(v * 128.f + 0.5f) << ","sv;
		}
	}
	print << "};"sv;
	print.Flush();

	print << "const u16 "sv << "indices"sv << "[] = {"sv;
	for (u16 i = 0; i < surf[0].num_tris; ++i) {
		auto& idx = tris[i].indices;
		print << idx[0] << ","sv << idx[1] << ","sv << idx[2] << ","sv;
	}
	print << "};"sv;
	print.Flush();
}

////////////////////////////////////////////////////////////////

int main() {
	Options options;

	options.md3_paths = {
		#define PP_DEMO_MODEL_PATH(name)		"../../demo/data/models/" #name ".md3",
		DEMO_MODELS(PP_DEMO_MODEL_PATH)
		#undef PP_DEMO_MODEL_PATH
	};
	options.out_path = "../../demo/cooked/models.h";

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
