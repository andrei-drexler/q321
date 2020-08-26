#pragma once

#include "cooked/cooked_shaders.h"

namespace Demo {
	static const u32 GPUPoolSizes[Gfx::Arena::Count] = {
		8  * Mem::MB,	// permanent
		16 * Mem::MB,	// level
		16 * Mem::MB,	// dynamic
	};

	////////////////////////////////////////////////////////////////

	enum Attrib : u8 {
		#define PP_DEMO_VERTEX_ATTRIB_ID(name, type)		name,
		DEMO_VERTEX_ATTRIBS(PP_DEMO_VERTEX_ATTRIB_ID)
		#undef PP_DEMO_VERTEX_ATTRIB_ID

		Count,

		#define PP_DEMO_VERTEX_ATTRIB_BIT(name, type)		name##Bit = 1 << name,
		DEMO_VERTEX_ATTRIBS(PP_DEMO_VERTEX_ATTRIB_BIT)
		#undef PP_DEMO_VERTEX_ATTRIB_BIT
	};

	////////////////////////////////////////////////////////////////

	namespace Shader {
		enum {
			MapVertexBits	= Attrib::PositionBit|Attrib::TexCoordBit|Attrib::NormalBit,
			ModelVertexBits	= Attrib::PositionBit|Attrib::NormalBit,
			FSVertexBits	= Attrib::PositionBit,
			UIVertexBits	= Attrib::PositionBit|Attrib::TexCoordBit|Attrib::ColorBit,
		};
		GFX_DECLARE_SHADERS(DEMO_SHADERS);
	}

	////////////////////////////////////////////////////////////////

	namespace Texture {
		enum : Gfx::Texture::ID {
			#define PP_DEMO_TEXTURE_ID(name, base, shader, width, height, format, flags)					name,
			DEMO_TEXTURES(PP_DEMO_TEXTURE_ID)
			#undef PP_DEMO_TEXTURE_ID
			Count,
		};

		static constexpr Gfx::Texture::Descriptor Descriptors[] = {
			#define PP_DEMO_TEXTURE_DESCRIPTOR(name, base, shader, width, height, format, flags)			{{{width, height}}, Gfx::Texture::Format::format, Gfx::Texture::Flags(flags)},
			DEMO_TEXTURES(PP_DEMO_TEXTURE_DESCRIPTOR)
			#undef PP_DEMO_TEXTURE_DESCRIPTOR
		};
		
		static constexpr Gfx::Shader::ID ProcGen[] = {
			#define PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR(name, base, shader, width, height, format, flags)	shader,
			DEMO_TEXTURES(PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR)
			#undef PP_DEMO_TEXTURE_PROCGEN_DESCRIPTOR
		};

		static constexpr Gfx::Texture::ID Base[] = {
			#define PP_DEMO_TEXTURE_BASE(name, base, shader, width, height, format, flags)	base,
			DEMO_TEXTURES(PP_DEMO_TEXTURE_BASE)
			#undef PP_DEMO_TEXTURE_BASE
		};

		/* Make sure base textures are initialized before their variations */
		#define PP_DEMO_TEXTURE_CHECK_ORDER(name, base, shader, width, height, format, flags)\
			static_assert(base == Gfx::InvalidID || base < name, "Invalid texture order: " #base " should be defined before " #name);
		DEMO_TEXTURES(PP_DEMO_TEXTURE_CHECK_ORDER)
		#undef PP_DEMO_TEXTURE_CHECK_ORDER

		void GenerateSolidTextures();
		void GenerateFont();
		void GenerateProceduralTextures();
	}

	////////////////////////////////////////////////////////////////

	vec2 g_fov; // in radians

	namespace Uniform {
		GFX_DECLARE_UNIFORMS(DEMO_UNIFORMS);

		using Metadata::Count;

		namespace Cache {
			mat4 ViewProj;
		}

		NOINLINE void SetModelMatrix(const mat4& mat) {
			World = mat;
			MVP = Cache::ViewProj * mat;
		}

		struct Pack {
			enum {
				#define PP_ADD_UNIFORM_SIZE(name, type, ...) +sizeof(type)
				BufferSize = DEMO_UNIFORMS(PP_ADD_UNIFORM_SIZE),
				#undef PP_ADD_UNIFORM_SIZE
			};

			u8		buffer[BufferSize];

			void	Acquire();
			void	Apply() const;
		};
	}

	////////////////////////////////////////////////////////////////

	enum {
		MAX_NUM_DRAWCALLS				= 4096,
	};

	struct DrawCall {
		u16								material;
		Uniform::Pack					uniforms;
		Gfx::Mesh						mesh;
	};

	u16									g_num_drawcalls;
	Array<DrawCall, MAX_NUM_DRAWCALLS>	g_drawcalls;
	Array<u16, MAX_NUM_DRAWCALLS>		g_drawcall_order;
	Array<u16, Material::Count>			g_num_material_drawcalls;

	void								AddDrawCall(Material::ID material, const Gfx::Mesh& mesh);
	void								FlushDrawCalls();

	////////////////////////////////////////////////////////////////

	static constexpr u8 MaterialShaders[] = {
		#define PP_DEMO_MATERIAL_SHADER(path, shader, texture, contents, draw, light)		Demo::Shader::shader,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_SHADER)
		#undef PP_DEMO_MATERIAL_SHADER
	};

	static constexpr u8 MaterialTextures[] = {
		#define PP_DEMO_MATERIAL_TEXTURE(path, shader, texture, contents, draw, light)		Demo::Texture::texture,
		DEMO_MATERIALS(PP_DEMO_MATERIAL_TEXTURE)
		#undef PP_DEMO_MATERIAL_TEXTURE
	};

	////////////////////////////////////////////////////////////////

	namespace UI {
		static constexpr float
			VirtualHalfWidth  = 512.f,
			VirtualHalfHeight = 512.f
		;
		static constexpr float VirtualAspectRatio = VirtualHalfWidth / VirtualHalfHeight;

		/* Fonts */
		
		enum Font {
			LargeFontBlurry,
			LargeFont,
			SmallFont,

			FontCount,
		};

		Sys::Font::Glyph			glyphs[FontCount][Sys::Font::Glyph::Count];
		const Sys::Font::Glyph&		GetGlyph(char c, UI::Font font = UI::SmallFont);

		i32							Measure(const char* text, Font font = LargeFont);
		void						Print(const char* text, const vec2& pos, vec2 scale = 1.f, u32 color = -1, float align = 0.f, Font font = SmallFont);
		void						PrintShadowed(const char* text, const vec2& pos, const vec2& scale = 1.f, u32 color = -1, float align = 0.f, Font font = SmallFont);

		constexpr char FontDescriptors[] =
			/*Size		Padding+1	Name*/
			"\x30"		"\x09"		"Impact"				"\0"
			"\x30"		"\x01"		"Impact"				"\0"
			"\x12"		"\x01"		"Courier New Bold"		"\0"
		;
		constexpr vec2 FontScale[FontCount] = {
			{1.5f, 1.f},
			{1.5f, 1.f},
			{1.f,  1.f},
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

			Gfx::SetRenderTarget(Gfx::Backbuffer, &Gfx::Clear::ColorAndDepth);

			if (frame == 0) {
				Gfx::SetShader(Shader::bglogo);
				Gfx::DrawFullScreen();

				static constexpr vec2
					font_scale = UI::FontScale[UI::LargeFont] * 0.75f,
					pos = {0.f, 128.f}
				;
				UI::PrintShadowed("Starting up...", pos, font_scale, -1, 0.5f, UI::LargeFont);
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
				assert(i0 < num_verts);
				assert(i1 < num_verts);
				assert(i2 < num_verts);
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
		const char* name = descriptor + 2;
		int size = descriptor[0];
		u16 padding = u16(descriptor[1] - 1);
		Sys::RasterizeFont(name, size, 0, font_pixels, UI::TexDescriptor.width, UI::TexDescriptor.height, padding, packer, UI::glyphs[font_index]);
	}

	// 2-pass box blur for the large, blurry font
	const u32 BlurRadius = 4;
	for (u32 axis = 0, axis_pitch = 1, cross_pitch = UI::TexDescriptor.width; axis < 2; ++axis, Swap(axis_pitch, cross_pitch)) {
		for (const Sys::Font::Glyph& glyph : UI::glyphs[UI::LargeFontBlurry]) {
			u32* data = font_pixels + glyph.box_min[1] * UI::TexDescriptor.width + glyph.box_min[0];

			for (u32 i = 0; i < glyph.box_size[!axis]; ++i, data += cross_pitch) {
				const u16 MaxGlyphSize = 256;
				assert(glyph.box_size[axis] <= MaxGlyphSize);

				u32 prefix_sum[MaxGlyphSize + 1];
				prefix_sum[0] = 0;
				for (u32 j = 0, *pixel = data; j < glyph.box_size[axis]; ++j, pixel += axis_pitch)
					prefix_sum[j + 1] = prefix_sum[j] + (*pixel & 0xff);

				for (u32 j = 0, *pixel = data; j < glyph.box_size[axis]; ++j, pixel += axis_pitch) {
					i32 left  = j - BlurRadius;
					i32 right = j + BlurRadius;
					assign_max(left, 0);
					assign_min(right, i32(glyph.box_size[axis]));

					u32 blurred = prefix_sum[right] - prefix_sum[left];
					blurred += blurred >> 1; // boost
					blurred /= BlurRadius * 2;
					assign_min(blurred, u32(255));

					*pixel = blurred * 0x01'01'01'01u;
				}
			}
		}
	}

	// Hack: remap large font lowercase glyphs to uppercase
	MemCopy(UI::glyphs[UI::LargeFont] + 'a' - Sys::Font::Glyph::Begin, UI::glyphs[UI::LargeFont] + 'A' - Sys::Font::Glyph::Begin, 'z' - 'a' + 1);
	MemCopy(UI::glyphs[UI::LargeFontBlurry] + 'a' - Sys::Font::Glyph::Begin, UI::glyphs[UI::LargeFontBlurry] + 'A' - Sys::Font::Glyph::Begin, 'z' - 'a' + 1);

	// 'Impact' is a decent match for the large Q3 font, but it's not perfect.
	// We can get somewhat closer with a few glyph spacing tweaks, though:

	// increase space glyph advance by 100%
	auto& large_space = UI::glyphs[UI::LargeFont][' ' - Sys::Font::Glyph::Begin];
	large_space.advance <<= 1;
	auto& large_space_blurry = UI::glyphs[UI::LargeFontBlurry][' ' - Sys::Font::Glyph::Begin];
	large_space_blurry.advance <<= 1;

	// increase advance a bit for all glyphs
	for (u16 i = 0; i < size(UI::glyphs[UI::LargeFont]); ++i) {
		++UI::glyphs[UI::LargeFont][i].advance;
		++UI::glyphs[UI::LargeFontBlurry][i].advance;
	}

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
		Uniform::Texture0 = Base[texture_id];
		Gfx::UpdateUniforms();
		Gfx::DrawFullScreen();
		Gfx::GenerateMipMaps(texture_id);
	}
}

////////////////////////////////////////////////////////////////

void Demo::Uniform::Pack::Acquire() {
	namespace Meta = Uniform::Metadata;

	for (u32 i = 0, offset = 0; i < Uniform::Count; ++i) {
		u32 size = Meta::Sizes[i];
		MemCopy(buffer + offset, Meta::Addresses[i], size);
		offset += size;
	}
}

void Demo::Uniform::Pack::Apply() const {
	namespace Meta = Uniform::Metadata;

	for (u32 i = 0, offset = 0; i < Uniform::Count; ++i) {
		u32 size = Meta::Sizes[i];
		MemCopy(Meta::Addresses[i], buffer + offset, size);
		offset += size;
	}
}

////////////////////////////////////////////////////////////////

NOINLINE void Demo::AddDrawCall(Material::ID material, const Gfx::Mesh& mesh) {
	assert(u32(material) < u32(Material::Count));

	if (g_num_drawcalls >= MAX_NUM_DRAWCALLS)
		FlushDrawCalls();

	DrawCall& call = g_drawcalls[g_num_drawcalls++];
	call.material = material;
	call.uniforms.Acquire();
	MemCopy(&call.mesh, &mesh);

	++g_num_material_drawcalls[material];
}

NOINLINE void Demo::FlushDrawCalls() {
	if (!g_num_drawcalls)
		return;

	/* transform counts to offsets */
	for (u32 material = 0, base = 0; material < Material::Count; ++material) {
		auto& count = g_num_material_drawcalls[material];
		u32 next = base + count;
		count = base;
		base = next;
	}

	/* fill order array */
	for (u32 i = 0; i < g_num_drawcalls; ++i) {
		const DrawCall& call = g_drawcalls[i];
		g_drawcall_order[g_num_material_drawcalls[call.material]++] = i;
	}

	/* execute draw calls */
	for (u32 i = 0; i < g_num_drawcalls; ++i) {
		const DrawCall& call = g_drawcalls[g_drawcall_order[i]];
		if (!call.mesh.num_vertices || !call.mesh.num_indices)
			continue;

		call.uniforms.Apply();
		
		Gfx::SetShader(MaterialShaders[call.material]);
		Gfx::UpdateUniforms();
		Gfx::Draw(call.mesh);
	}

	/* reset state */
	MemSet(&g_num_material_drawcalls[0], 0, size(g_num_material_drawcalls));
	g_num_drawcalls = 0;
}


////////////////////////////////////////////////////////////////

NOINLINE const Sys::Font::Glyph& Demo::UI::GetGlyph(char c, Font font) {
	u8 index = u8(c - Sys::Font::Glyph::Begin);
	if (index >= Sys::Font::Glyph::Count)
		index = 0;
	return glyphs[font][index];
}

NOINLINE i32 Demo::UI::Measure(const char* text, Font font) {
	i32 total = 0;
	while (*text)
		total += GetGlyph(*text++, font).advance;
	return total;
}

NOINLINE void Demo::UI::Print(const char* text, const vec2& pos, vec2 scale, u32 color, float align, Font font) {
	vec2 cursor;
	cursor.x = pos.x - align * Measure(text, font) * scale.x;
	cursor.y = pos.y;

	u32 num_chars = StrLen(text);

	while (num_chars) {
		i32 batch_chars = min<i32>(num_chars, MAX_NUM_QUADS);
		num_chars -= batch_chars;
		
		VertexFormat* v = AddQuads(batch_chars);
		
		while (batch_chars > 0) {
			auto& glyph = GetGlyph(*text++, font);
			--batch_chars;

			for (u32 i = 0; i < 4; ++i, ++v) {
				u32 d[2];
				d[0] = (6 >> i) & 1;	// 0 1 1 0
				d[1] = i >> 1;		// 0 0 1 1
				for (u16 j = 0; j < 2; ++j) {
					v[0].pos[j] = cursor[j] + glyph.anchor[j] * scale[j] + (glyph.box_size[j] * d[j]) * scale[j];
					v[0].uv[j] = (glyph.box_min[j] + (glyph.box_size[j] * d[j])) / float(TexDescriptor.size[j]);
				}
				v[0].color = color;
			}

			cursor.x += glyph.advance * scale[0];
		}
	}
}

NOINLINE void Demo::UI::PrintShadowed(const char* text, const vec2& pos, const vec2& scale, u32 color, float align, Font font) {
	for (u16 pass = 0; pass < 2; ++pass) {
		u32 pass_color = color;
		vec2 cursor = pos;
		if (!pass) {
			float shadow_dist = 4.f * scale.y;
			cursor.x += shadow_dist;
			cursor.y -= shadow_dist;
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
			#define PP_ADD_MEMBER(name, normalized)\
				{OFFSET_OF(VertexFormat, name), Gfx::Vertex::TypeToEnum<decltype(vertices[0].name)>, normalized, sizeof(vertices[0])}

			PP_ADD_MEMBER(pos,   false),
			PP_ADD_MEMBER(uv,    false),
			{},
			PP_ADD_MEMBER(color, true),

			#undef PP_ADD_MEMBER
		},
	};
	static_assert(0 == Attrib::Position,	"Invalid BaseMesh position stream index");
	static_assert(1 == Attrib::TexCoord,	"Invalid BaseMesh texcoord stream index");
	static_assert(3 == Attrib::Color,		"Invalid BaseMesh color stream index");

	u32 num_vertices = num_quads * 4;
	u32 num_indices = num_quads * 6;

	float real_aspect = Gfx::GetAspectRatio();

	vec2 scale;
	if (real_aspect > UI::VirtualAspectRatio) {
		scale.x = 1.f / VirtualHalfHeight / real_aspect;
		scale.y = 1.f / VirtualHalfHeight;
	} else {
		scale.x = 1.f / VirtualHalfWidth;
		scale.y = real_aspect / VirtualHalfWidth;
	}

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
