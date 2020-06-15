#pragma once

#include "cooked/cooked_shaders.h"

namespace Demo {
	enum Attrib : u8 {
		#define PP_DEMO_VERTEX_ATTRIB_ID(name, type)		name,
		DEMO_VERTEX_ATTRIBS(PP_DEMO_VERTEX_ATTRIB_ID)
		#undef PP_DEMO_VERTEX_ATTRIB_ID

		Count,

		#define PP_DEMO_VERTEX_ATTRIB_BIT(name, type)		name##Bit = 1 << name,
		DEMO_VERTEX_ATTRIBS(PP_DEMO_VERTEX_ATTRIB_BIT)
		#undef PP_DEMO_VERTEX_ATTRIB_BIT
	};

	namespace Shader {
		enum {
			MapVertexBits	= Attrib::PositionBit|Attrib::TexCoordBit|Attrib::NormalBit,
			FSVertexBits	= Attrib::PositionBit,
			UIVertexBits	= Attrib::PositionBit|Attrib::TexCoordBit|Attrib::ColorBit,
		};
		GFX_DECLARE_SHADERS(DEMO_SHADERS);
	}

	namespace Texture {
		enum : Gfx::Texture::ID {
			#define PP_DEMO_TEXTURE_ID(name, shader, width, height, format, flags)					name,
			DEMO_TEXTURES(PP_DEMO_TEXTURE_ID)
			#undef PP_DEMO_TEXTURE_ID
			Count,
		};

		static constexpr Gfx::Texture::Descriptor Descriptors[] = {
			#define PP_DEMO_TEXTURE_DESCRIPTOR(name, shader, width, height, format, flags)			{width, height, Gfx::Texture::Format::format, Gfx::Texture::Flags(flags)},
			DEMO_TEXTURES(PP_DEMO_TEXTURE_DESCRIPTOR)
			#undef PP_DEMO_TEXTURE_DESCRIPTOR
		};
		
		static constexpr Gfx::Shader::ID ProcGen[] = {
			#define PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR(name, shader, width, height, format, flags)	shader,
			DEMO_TEXTURES(PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR)
			#undef PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR
		};

		void GenerateSolidTextures();
		void GenerateFont();
		void GenerateProceduralTextures();
	}

	namespace Uniform {
		GFX_DECLARE_UNIFORMS(DEMO_UNIFORMS);
	}

	namespace UI {
		/* Fonts */
		
		enum Font {
			LargeFont,
			SmallFont,

			FontCount,
		};

		vec2 GetScale()				{ return {float(Sys::g_window.width)/1920.f, float(Sys::g_window.height)/1080.f}; }

		Sys::Font::Glyph			glyphs[FontCount][Sys::Font::Glyph::Count];
		const Sys::Font::Glyph&		GetGlyph(char c, UI::Font font = UI::SmallFont);

		i32							Measure(const char* text, Font font = LargeFont);
		void						Print(const char* text, const vec2& pos, const vec2& scale = 1.f, u32 color = -1, float align = 0.f, Font font = SmallFont);
		void						PrintShadowed(const char* text, const vec2& pos, const vec2& scale = 1.f, u32 color = -1, float align = 0.f, Font font = SmallFont);

		constexpr char FontDescriptors[] =
			"\x30" "Impact"					"\0"
			"\x10" "Courier New Bold"		"\0"
		;
		constexpr vec2 FontScale[FontCount] = {
			{1.5f, 1.f},
			{1.f, 1.f},
		};

		constexpr auto TexDescriptor = Texture::Descriptors[Texture::Font];
		
		/* Geometry buffers */

		enum {
			MAX_NUM_QUADS		= 2048,
			MAX_NUM_VERTICES	= MAX_NUM_QUADS * 4,
			MAX_NUM_INDICES		= MAX_NUM_QUADS * 6,
		};

		struct VertexFormat {
			vec2		pos;
			vec2		uv;
			u32			color;
		};

		u16				num_quads;
		VertexFormat	vertices[MAX_NUM_VERTICES];
		u32				indices[MAX_NUM_INDICES];

		VertexFormat*	AddQuads(u16 count);
		void			FlushGeometry();
		void			InitIndices();
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void UI::InitIndices() {
		constexpr u8 QuadVertexOrder[6] = {
			0, 1, 2,
			0, 2, 3,
		};

		for (u16 i = 0, v = 0; i < UI::MAX_NUM_INDICES; v += 4) {
			for (u16 j = 0; j < 6; ++j)
				UI::indices[i++] = v + QuadVertexOrder[j];
		}
	}

	FORCEINLINE void InitGfxResources() {
		/* initialize essential resources */
		UI::InitIndices();
		Gfx::RegisterTextures(Texture::Descriptors);
		Texture::GenerateSolidTextures();
		Texture::GenerateFont();
		Uniform::RegisterAll();
		Shader::RegisterAll(g_vertex_shaders, g_fragment_shaders);
		Gfx::CompileShaders(0, Shader::UI + 1);

		/* show a basic loading screen while compiling shaders */
		for (i8 frame = 4; frame >= 0; --frame) {
			// HACK: we hide the inevitable OpenGL black frame flickering
			// on startup for 'borderless full-screen' windows by drawing
			// a few fully black frames before the actual loading screen

			Gfx::SetRenderTarget(Gfx::Backbuffer);
			Gfx::Clear(Gfx::ClearBit::ColorAndDepth);

			if (frame == 0) {
				Gfx::SetShader(Shader::bglogo);
				Gfx::DrawFullScreen();

				vec2 pos = Gfx::GetResolution() * vec2{0.5f, 0.375f};
				vec2 ui_scale = UI::GetScale() * 0.75f;
				vec2 font_scale = UI::FontScale[UI::LargeFont] * ui_scale.y;
				UI::PrintShadowed("starting up...", pos, font_scale, -1, 0.5f, UI::LargeFont);
				UI::FlushGeometry();
			}

			Gfx::Present();
		}

		/* compile remaining shaders */
		Gfx::CompileShaders(Shader::UI + 1, Shader::Count - Shader::UI - 1);

		Texture::GenerateProceduralTextures();
#ifdef SAVE_TEXTURE
		Gfx::SaveTGA(PP_STRINGIZE(SAVE_TEXTURE) ".tga", Demo::Texture::SAVE_TEXTURE);
#endif
	}

	NOINLINE void ComputeNormals(const vec3* positions, const u32* indices, u32 num_verts, u32 num_indices, vec3* normals) {
		MemSet(normals, 0, num_verts);

		for (u32 i = 0; i < num_indices; i += 3, indices += 3) {
			constexpr u8 Next[] = {1, 2, 0, 1};
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = indices[j];
				u32 i1 = indices[Next[j]];
				u32 i2 = indices[Next[j + 1]];
				normals[i0] += cross(positions[i1] - positions[i0], positions[i2] - positions[i1]);
			}
		}

		for (u32 i = 0; i < num_verts; ++i)
			safe_normalize(normals[i]);
	}
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Texture::GenerateSolidTextures() {
	for (u16 i = 0; i < 2; ++i) {
		const u32 WhiteTextureSize = Descriptors[Texture::White].width * Descriptors[Texture::White].height;
		u32 white[WhiteTextureSize];
		MemSet32(white, (255 >> i) * 0x01'01'01 | 0xFF'00'00'00, WhiteTextureSize);
		Gfx::SetTextureContents(Texture::White + i, white);
		Gfx::GenerateMipMaps(Texture::White + i);
	}
}

FORCEINLINE void Demo::Texture::GenerateFont() {
	u32* font_pixels = Sys::Alloc<u32>(UI::TexDescriptor.width * UI::TexDescriptor.height);

	RectPacker packer;
	packer.Init(UI::TexDescriptor.width, UI::TexDescriptor.height);

	u8 font_index = 0;
	for (const char* descriptor = UI::FontDescriptors; *descriptor; descriptor = NextAfter(descriptor), ++font_index) {
		Sys::RasterizeFont(descriptor + 1, descriptor[0], 0, font_pixels, UI::TexDescriptor.width, UI::TexDescriptor.height, packer, UI::glyphs[font_index]);
	}

	// Hack: remap large font lowercase glyphs to uppercase
	MemCopy(UI::glyphs[UI::LargeFont] + 'a' - Sys::Font::Glyph::Begin, UI::glyphs[UI::LargeFont] + 'A' - Sys::Font::Glyph::Begin, 'z' - 'a' + 1);

	// 'Impact' is a decent match for the large Q3 font, but it's not perfect.
	// We can get somewhat closer with a few glyph spacing tweaks, though:

	// increase space glyph advance by 100%
	auto& large_space = UI::glyphs[UI::LargeFont][' ' - Sys::Font::Glyph::Begin];
	large_space.advance <<= 1;

	// increase advance a bit for all glyphs
	for (u16 i = 0; i < size(UI::glyphs[UI::LargeFont]); ++i)
		++UI::glyphs[UI::LargeFont][i].advance;

	if constexpr (0) {
		Gfx::SaveTGA("font.tga", font_pixels, UI::TexDescriptor.width, UI::TexDescriptor.height);
	}
	Gfx::SetTextureContents(Texture::Font, font_pixels);
	Gfx::GenerateMipMaps(Texture::Font);

	Sys::Free(font_pixels);
}

FORCEINLINE void Demo::Texture::GenerateProceduralTextures() {
	for (u32 texture_id = 0; texture_id < Texture::Count; ++texture_id) {
		auto& shader = ProcGen[texture_id];
		if (shader == Gfx::InvalidID)
			continue;

		Gfx::SetRenderTarget(texture_id);
		Gfx::SetShader(shader);
		Gfx::DrawFullScreen();
		Gfx::GenerateMipMaps(texture_id);
	}
}

////////////////////////////////////////////////////////////////

const Sys::Font::Glyph& Demo::UI::GetGlyph(char c, Font font) {
	u8 index = u8(c - Sys::Font::Glyph::Begin);
	if (index >= Sys::Font::Glyph::Count)
		index = 0;
	return glyphs[font][index];
}

i32 Demo::UI::Measure(const char* text, Font font) {
	i32 total = 0;
	while (*text)
		total += GetGlyph(*text++, font).advance;
	return total;
}

void Demo::UI::Print(const char* text, const vec2& pos, const vec2& scale, u32 color, float align, Font font) {
	vec2 cursor = pos;
	cursor.x -= align * Measure(text, font) * scale.x;

	u32 num_chars = StrLen(text);

	while (num_chars) {
		i32 batch_chars = min<i32>(num_chars, MAX_NUM_QUADS);
		num_chars -= batch_chars;
		
		VertexFormat* v = AddQuads(batch_chars);
		
		while (batch_chars > 0) {
			auto& glyph = GetGlyph(*text++, font);
			--batch_chars;

			for (u16 i = 0; i < 4; ++i, ++v) {
				u16 dx = (6 >> i) & 1;	// 0 1 1 0
				u16 dy = i >> 1;		// 0 0 1 1
				v[0].pos[0] = cursor[0] + glyph.anchor[0] * scale[0] + (glyph.box_size[0] * dx) * scale[0];
				v[0].pos[1] = cursor[1] - glyph.anchor[1] * scale[1] - (glyph.box_size[1] * dy) * scale[1];
				v[0].uv[0] = (glyph.box_min[0] + (glyph.box_size[0] * dx)) / float(TexDescriptor.width);
				v[0].uv[1] = (glyph.box_min[1] + (glyph.box_size[1] * dy)) / float(TexDescriptor.height);
				v[0].color = color;
			}

			cursor.x += glyph.advance * scale[0];
		}
	}
}

void Demo::UI::PrintShadowed(const char* text, const vec2& pos, const vec2& scale, u32 color, float align, Font font) {
	for (u16 pass = 0; pass < 2; ++pass) {
		u32 pass_color = color;
		vec2 cursor = pos;
		if (!pass) {
			cursor += 6.f * GetScale().y;
			pass_color &= 0xFF000000;
		}
		Print(text, cursor, scale, pass_color, align, font);
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE Demo::UI::VertexFormat* Demo::UI::AddQuads(u16 count) {
	assert(count < MAX_NUM_QUADS);
	if (count + num_quads > MAX_NUM_QUADS)
		FlushGeometry();

	VertexFormat* first_vertex = vertices + num_quads * 4;
	num_quads += count;
	return first_vertex;
}

NOINLINE void Demo::UI::FlushGeometry() {
	if (!num_quads)
		return;
	assert(num_quads <= MAX_NUM_QUADS);

	static constexpr Gfx::Mesh BaseMesh = {
		{
			{OFFSET_OF(VertexFormat, pos),		Gfx::Vertex::TypeToEnum<vec3>,	false, sizeof(vertices[0])},
			{OFFSET_OF(VertexFormat, uv),		Gfx::Vertex::TypeToEnum<vec2>,	false, sizeof(vertices[0])},
			{},
			{OFFSET_OF(VertexFormat, color),	Gfx::Vertex::TypeToEnum<u32>,	true, sizeof(vertices[0])},
		},
	};
	static_assert(0 == Attrib::Position,	"Invalid BaseMesh position stream index");
	static_assert(1 == Attrib::TexCoord,	"Invalid BaseMesh texcoord stream index");
	static_assert(3 == Attrib::Color,		"Invalid BaseMesh color stream index");

	u32 num_vertices = num_quads * 4;
	u32 num_indices = num_quads * 6;

	vec2 scale = 1.f / Gfx::GetResolution();
	for (u16 i = 0, count = num_vertices; i < count; ++i)
		vertices[i].pos *= scale;

	Gfx::Mesh mesh;
	MemCopy(&mesh, &BaseMesh);

	u32 vertex_addr = Gfx::UploadGeometry(vertices, num_vertices);
	for (u16 i = 0; i < Attrib::Count; ++i)
		if (mesh.vertices[i].stride)
			mesh.vertices[i].addr += vertex_addr;

	mesh.index_addr		= Gfx::UploadGeometry(indices, num_indices);
	mesh.num_vertices	= num_vertices;
	mesh.num_indices	= num_indices;

	Gfx::SetShader(Shader::UI);
	Uniform::Texture0 = Texture::Font;

	Gfx::UpdateUniforms();
	Gfx::Draw(mesh);

	num_quads = 0;
}
