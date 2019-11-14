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
	};

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RegisterGfxResources() {
		Uniform::RegisterAll();
		Gfx::RegisterTextures(Texture::Descriptors);
		Shader::RegisterAll(g_vertex_shaders, g_fragment_shaders);
	}
}

FORCEINLINE void Demo::Texture::GenerateAll() {
	const u32 WhiteTextureSize = Descriptors[Texture::White].width * Descriptors[Texture::White].height;
	for (u16 i = 0; i < 2; ++i) {
		u32 white[WhiteTextureSize];
		MemSet(white, 255 >> i, sizeof(white));
		Gfx::SetTextureContents(Texture::White + i, white);
		Gfx::GenerateMipMaps(Texture::White + i);
	}

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