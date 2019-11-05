#pragma once

namespace Demo {
	namespace Material {
		enum Flags : u8 {
			Opaque			= 0 << 0,
			Translucent		= 1 << 0,
			Invisible		= 2 << 0,
			Sky				= 3 << 0,
			MaskVisibility	= 3 << 0,

			NonSolid		= 0 << 2,
			NoDrop			= 1 << 2,
			PlayerClip		= 2 << 2,
			WeaponClip		= 3 << 2,
			Solid			= WeaponClip,
			MaskContents	= 3 << 2,

			NeedsUV			= 1 << 4,
		};

		////////////////////////////////////////////////////////////////

		static constexpr u8 Properties[] = {
			#define PP_DEMO_MATERIAL_FLAGS(path, shader, texture, contents, draw, light)			Material::contents | Material::draw,
			DEMO_MATERIALS(PP_DEMO_MATERIAL_FLAGS)
			#undef PP_DEMO_MATERIAL_FLAGS
		};

		constexpr u32 Version = Hash(
			#define PP_DEMO_HASH_MATERIAL_NAME(path, shader, texture, contents, draw, light)		path "*"
			DEMO_MATERIALS(PP_DEMO_HASH_MATERIAL_NAME) "\0"
			#undef PP_DEMO_HASH_MATERIAL_NAME
		);

		static constexpr const char* Paths[] = {
			#define PP_DEMO_MATERIAL_PATH(path, shader, texture, contents, draw, light)				path,
			DEMO_MATERIALS(PP_DEMO_MATERIAL_PATH)
			#undef PP_DEMO_MATERIAL_PATH
		};

		////////////////////////////////////////////////////////////////
		
		constexpr u32 PackLight(u32 intensity, u32 r, u32 g, u32 b) {
			return (r << 0) | (g << 5) | (b << 10) | (intensity << 15);
		}

		constexpr u32 PackLight(u32 r, u32 g, u32 b) {
			u32 intensity = Math::max(r, Math::max(g, b));
			if (!intensity)
				return 0;
			r = (r * 31 + intensity / 2) / intensity;
			g = (g * 31 + intensity / 2) / intensity;
			b = (b * 31 + intensity / 2) / intensity;
			return PackLight(intensity, r, g, b);
		}

		constexpr u32 PackLight(u32 intensity) {
			return PackLight(intensity, intensity, intensity);
		}

		struct SurfaceLight {
			u16 intensity;
			vec3 color;
		};

		FORCEINLINE SurfaceLight UnpackSurfaceLight(u32 packed) {
			SurfaceLight light;
			light.intensity = packed >> 15;
			light.color.r = (packed >>  0) & 31;
			light.color.g = (packed >>  5) & 31;
			light.color.b = (packed >> 10) & 31;
			light.color /= 31.f;
			return light;
		}

		static constexpr u32 Lights[] = {
			#define PP_DEMO_MATERIAL_LIGHT(path, shader, texture, contents, draw, light)			PackLight light,
			DEMO_MATERIALS(PP_DEMO_MATERIAL_LIGHT)
			#undef PP_DEMO_MATERIAL_LIGHT
		};
	}
}
