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
			MapVertexBits = Attrib::PositionBit|Attrib::TexCoordBit|Attrib::NormalBit,
			FSVertexBits = Attrib::PositionBit,
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
		enum {
			LargeFont,
			SmallFont,

			FontCount,
		};

		constexpr char FontDescriptors[] =
			"\x20" "Impact"					"\0"
			"\x12" "Lucida Console Bold"	"\0"
		;

		Sys::Font::Glyph	glyphs[FontCount][Sys::Font::Glyph::Count];
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RegisterGfxResources() {
		Uniform::RegisterAll();
		Gfx::RegisterTextures(Texture::Descriptors);
		Shader::RegisterAll(g_vertex_shaders, g_fragment_shaders);
	}
}

FORCEINLINE void Demo::Texture::GenerateAll() {
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
	constexpr auto FontTexDescriptor = Descriptors[Texture::Font];
	u32* font_pixels = Sys::Alloc<u32>(FontTexDescriptor.width * FontTexDescriptor.height);

	RectPacker packer;
	packer.Init(FontTexDescriptor.width, FontTexDescriptor.height);

	u8 font_index = 0;
	for (const char* descriptor = UI::FontDescriptors; *descriptor; descriptor = NextAfter(descriptor), ++font_index) {
		Sys::RasterizeFont(descriptor + 1, descriptor[0], 0, font_pixels, FontTexDescriptor.width, FontTexDescriptor.height, packer, UI::glyphs[font_index]);
	}
	//Gfx::SaveTGA("font.tga", font_pixels, FontTexDescriptor.width, FontTexDescriptor.height);
	Gfx::SetTextureContents(Texture::Font, font_pixels);
	Gfx::GenerateMipMaps(Texture::Font);

	Sys::Free(font_pixels);
}