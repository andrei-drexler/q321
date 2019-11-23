#pragma once

#include "cooked/shaders.h"

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

		void GenerateAll();
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
		u16				indices[MAX_NUM_INDICES];

		VertexFormat*	AddQuads(u16 count);
		void			FlushGeometry();
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RegisterGfxResources() {
		Uniform::RegisterAll();
		Gfx::RegisterTextures(Texture::Descriptors);
		Shader::RegisterAll(g_vertex_shaders, g_fragment_shaders);
		
		constexpr u8 QuadVertexOrder[6] = {
			0, 1, 2,
			0, 2, 3,
		};
		
		for (u16 i = 0, v = 0; i < UI::MAX_NUM_INDICES; v += 4) {
			for (u16 j = 0; j < 6; ++j)
				UI::indices[i++] = v + QuadVertexOrder[j];
		}
	}
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

NOINLINE void Demo::Texture::GenerateAll() {
	/* solid color textures */
	for (u16 i = 0; i < 2; ++i) {
		const u32 WhiteTextureSize = Descriptors[Texture::White].width * Descriptors[Texture::White].height;
		u32 white[WhiteTextureSize];
		MemSet(white, 255 >> i, sizeof(white));
		Gfx::SetTextureContents(Texture::White + i, white);
		Gfx::GenerateMipMaps(Texture::White + i);
	}

	/* procedural textures */
	for (u32 texture_id = 0; texture_id < Texture::Count; ++texture_id) {
		auto& shader = ProcGen[texture_id];
		if (shader == Gfx::InvalidID)
			continue;
		
		Gfx::SetRenderTarget(texture_id);
		Gfx::SetShader(shader);
		Gfx::DrawFullScreen();
		Gfx::GenerateMipMaps(texture_id);
	}

	/* font texture/glyph data */
	u32* font_pixels = Sys::Alloc<u32>(UI::TexDescriptor.width * UI::TexDescriptor.height);

	RectPacker packer;
	packer.Init(UI::TexDescriptor.width, UI::TexDescriptor.height);

	u8 font_index = 0;
	for (const char* descriptor = UI::FontDescriptors; *descriptor; descriptor = NextAfter(descriptor), ++font_index) {
		Sys::RasterizeFont(descriptor + 1, descriptor[0], 0, font_pixels, UI::TexDescriptor.width, UI::TexDescriptor.height, packer, UI::glyphs[font_index]);
	}

	// 'Impact' is decent match for the large Q3 font, but it's not perfect.
	// We can get somewhat closer with a few glyph spacing tweaks, though:

	// increase space glyph advance by 100%
	auto& large_space = UI::glyphs[UI::LargeFont][' ' - Sys::Font::Glyph::Begin];
	large_space.advance <<= 1;

	// increase advance a bit for all glyphs
	for (u16 i = 0; i < size(UI::glyphs[UI::LargeFont]); ++i)
		++UI::glyphs[UI::LargeFont][i].advance;

	//Gfx::SaveTGA("font.tga", font_pixels, FontTexDescriptor.width, FontTexDescriptor.height);
	Gfx::SetTextureContents(Texture::Font, font_pixels);
	Gfx::GenerateMipMaps(Texture::Font);

	Sys::Free(font_pixels);
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
			{&vertices[0].pos,		false,	sizeof(vertices[0])},
			{&vertices[0].uv,		false,	sizeof(vertices[0])},
			{},
			{&vertices[0].color,	true,	sizeof(vertices[0])},
		},
		indices,
	};
	static_assert(0 == Attrib::Position,	"Invalid BaseMesh position stream index");
	static_assert(1 == Attrib::TexCoord,	"Invalid BaseMesh texcoord stream index");
	static_assert(3 == Attrib::Color,		"Invalid BaseMesh color stream index");

	Gfx::Mesh mesh;
	MemCopy(&mesh, &BaseMesh);

	mesh.num_vertices	= num_quads * 4;
	mesh.num_indices	= num_quads * 6;

	vec2 scale = 1.f / Gfx::GetResolution();
	for (u16 i = 0, count = mesh.num_vertices; i < count; ++i)
		vertices[i].pos *= scale;

	Gfx::SetShader(Shader::UI);
	Uniform::Texture0 = Texture::Font;

	Gfx::UpdateUniforms();
	Gfx::Draw(mesh);

	num_quads = 0;
}
