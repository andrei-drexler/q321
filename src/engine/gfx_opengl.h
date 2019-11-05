#pragma once

////////////////////////////////////////////////////////////////

#include <GL/GL.h>
#pragma comment(lib, "opengl32.lib")

using GLchar = char;
using GLsizeiptr = iptr;
using GLintptr = iptr;

#define GL_TEXTURE_BASE_LEVEL             0x813C
#define GL_TEXTURE_MAX_LEVEL              0x813D
#define GL_BGRA                           0x80E1

#define GL_FRAGMENT_SHADER                0x8B30
#define GL_VERTEX_SHADER                  0x8B31
#define GL_COMPILE_STATUS                 0x8B81
#define GL_LINK_STATUS                    0x8B82
#define GL_INFO_LOG_LENGTH                0x8B84

#define GL_TEXTURE0                       0x84C0

#define GL_HALF_FLOAT                     0x140B
#define GL_RGBA32F                        0x8814
#define GL_RGB32F                         0x8815
#define GL_RGBA16F                        0x881A
#define GL_RGB16F                         0x881B
#define GL_DEPTH_COMPONENT32F             0x8CAC

#define GL_FRAMEBUFFER_COMPLETE           0x8CD5
#define GL_COLOR_ATTACHMENT0              0x8CE0
#define GL_DEPTH_ATTACHMENT               0x8D00
#define GL_FRAMEBUFFER                    0x8D40

#define GL_ARRAY_BUFFER                   0x8892
#define GL_ELEMENT_ARRAY_BUFFER           0x8893
#define GL_UNIFORM_BUFFER                 0x8A11
#define GL_READ_ONLY                      0x88B8
#define GL_WRITE_ONLY                     0x88B9
#define GL_READ_WRITE                     0x88BA
#define GL_STREAM_DRAW                    0x88E0
#define GL_STREAM_READ                    0x88E1
#define GL_STREAM_COPY                    0x88E2
#define GL_STATIC_DRAW                    0x88E4
#define GL_STATIC_READ                    0x88E5
#define GL_STATIC_COPY                    0x88E6
#define GL_DYNAMIC_DRAW                   0x88E8
#define GL_DYNAMIC_READ                   0x88E9
#define GL_DYNAMIC_COPY                   0x88EA

////////////////////////////////////////////////////////////////

#ifdef DEV
#define GL_DEV_FUNCTIONS(x)	\
	x(void,			DeleteBuffers, (GLsizei n, const GLuint *buffers))\
	x(void,			DeleteProgram, (GLuint program))\
	x(void,			DeleteShader, (GLuint shader))\
	x(void,			DetachShader, (GLuint program, GLuint shader))\
	x(void,			DeleteFramebuffers, (GLsizei n, const GLuint *framebuffers))\
	x(void,			GetProgramInfoLog, (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog))\
	x(void,			GetShaderInfoLog, (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog))\
	x(void,			GetShaderiv, (GLuint shader, GLenum pname, GLint *params))\
	x(GLenum,		CheckFramebufferStatus, (GLenum target))\
	/**/
#else
#define GL_DEV_FUNCTIONS(x)
#endif

#define GL_FUNCTIONS(x)	\
	x(void,			GenBuffers, (GLsizei n, GLuint *buffers))\
	x(void,			BindBuffer, (GLenum target, GLuint buffer))\
	x(void,			BufferData, (GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage))\
	x(void,			BufferSubData, (GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid *data))\
	x(GLuint,		CreateProgram, (void))\
	x(void,			GetProgramiv, (GLuint program, GLenum pname, GLint *params))\
	x(void,			UseProgram, (GLuint program))\
	x(void,			LinkProgram, (GLuint program))\
	x(GLuint,		CreateShader, (GLenum type))\
	x(void,			ShaderSource, (GLuint shader, GLsizei count, const GLchar* const *string, const GLint *length))\
	x(void,			CompileShader, (GLuint shader))\
	x(void,			AttachShader, (GLuint program, GLuint shader))\
	x(void,			EnableVertexAttribArray, (GLuint index))\
	x(void,			DisableVertexAttribArray, (GLuint index))\
	x(void,			VertexAttribPointer, (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid *pointer))\
/*	x(void,			BindAttribLocation, (GLuint program, GLuint index, const GLchar *name))*/\
/*	x(void,			GetActiveUniform, (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name))*/\
	x(GLint,		GetUniformLocation, (GLuint program, const GLchar *name))\
	x(void,			Uniform1i, (GLint location, GLint v0))\
	x(void,			Uniform4fv, (GLint location, GLsizei count, const GLfloat *value))\
	x(void,			UniformMatrix4fv, (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value))\
/*	x(GLuint,		GetUniformBlockIndex, (GLuint program, const GLchar *uniformBlockName))*/\
/*	x(void,			UniformBlockBinding, (GLuint program, GLuint uniformBlockIndex, GLuint uniformBlockBinding))*/\
	x(void,			ActiveTexture, (GLenum texture))\
	x(void,			GenerateMipmap, (GLenum target))\
	x(void,			BindFramebuffer, (GLenum target, GLuint framebuffer))\
	x(void,			GenFramebuffers, (GLsizei n, GLuint *framebuffers))\
	x(void,			FramebufferTexture2D, (GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level))\
/*	x(void,			DrawBuffers, (GLsizei n, const GLenum *bufs))*/\
	GL_DEV_FUNCTIONS(x)

////////////////////////////////////////////////////////////////

#define PP_APPEND_FUNC_NAME(ret, name, args) "gl" #name "\0"
#define PP_COUNT_FUNC(ret, name, args) 1 +
#define PP_ADD_GL_FUNC(ret, name, args) ret (APIENTRY *gl##name)args;

namespace GL {
	constexpr u32 NumFunctions = GL_FUNCTIONS(PP_COUNT_FUNC) 0;
	constexpr const char* FunctionNames = GL_FUNCTIONS(PP_APPEND_FUNC_NAME) "\0";
}

static union {
	struct {
		GL_FUNCTIONS(PP_ADD_GL_FUNC)
	};
	void* GLFunctions[GL::NumFunctions];
};

#undef PP_APPEND_FUNC_NAME
#undef PP_COUNT_FUNC
#undef PP_ADD_GL_FUNC

////////////////////////////////////////////////////////////////

namespace GL {
	static FORCEINLINE bool LoadFunctions() {
		const char* name = FunctionNames;
		for (u32 i = 0; i < NumFunctions; ++i, name = NextAfter(name)) {
			GLFunctions[i] = wglGetProcAddress(name);
			if (!GLFunctions[i]) {
				Sys::DebugStream << "ERROR: Could not locate function " << name << "\n";
				return false;
			}
		}
		return true;
	}

	////////////////////////////////////////////////////////////////

	static constexpr u32 GLVersion(u16 major, u16 minor) {
		return (major << 16) | minor;
	}

	static constexpr auto MinVersion = GLVersion(3, 3);

	static FORCEINLINE bool CheckVersion() {
		auto s = (const char*)glGetString(GL_VERSION);
		u16 major = 0, num;
	loop:
		num = 0;
		do { num = num * 10 + (*s++ - '0'); } while (u8(*s - '0') < 10);
		if (major == 0 && num != 0) {
			major = num;
			++s; // skip dot
			goto loop;
		}
		return GLVersion(major, num) >= GL::MinVersion;
	}

	////////////////////////////////////////////////////////////////

	static constexpr inline GLint GetInternalFormat(Gfx::Texture::Format format) {
		switch (format) {
			case Gfx::Texture::Format::BGRA8:	return GL_RGBA8;
			case Gfx::Texture::Format::RGBA16F:	return GL_RGBA16F;
			case Gfx::Texture::Format::RGBA32F:	return GL_RGBA32F;
			case Gfx::Texture::Format::Z32F:	return GL_DEPTH_COMPONENT32F;
			default:							return GL_INVALID_ENUM;
		}
	}

	static constexpr inline GLint GetFormat(Gfx::Texture::Format format) {
		switch (format) {
			case Gfx::Texture::Format::BGRA8:	return GL_BGRA;
			case Gfx::Texture::Format::RGBA16F:	return GL_RGBA;
			case Gfx::Texture::Format::RGBA32F:	return GL_RGBA;
			case Gfx::Texture::Format::Z32F:	return GL_DEPTH_COMPONENT;
			default:							return GL_INVALID_ENUM;
		}
	}

	static constexpr inline GLint GetType(Gfx::Texture::Format format) {
		switch (format) {
			case Gfx::Texture::Format::BGRA8:	return GL_UNSIGNED_BYTE;
			case Gfx::Texture::Format::RGBA16F:	return GL_HALF_FLOAT;
			case Gfx::Texture::Format::RGBA32F:	return GL_FLOAT;
			case Gfx::Texture::Format::Z32F:	return GL_FLOAT;
			default:							return GL_INVALID_ENUM;
		}
	}

	struct BlendFactors { GLenum src, dst; };

	static constexpr inline BlendFactors GetBlendFactors(Gfx::Shader::Flags flags) {
		using namespace Gfx::Shader;
		switch (flags & Flags::MaskBlend) {
			case Flags::Opaque:			return {GL_ONE, GL_ZERO};
			case Flags::Premultiplied:	return {GL_ONE, GL_ONE_MINUS_SRC_ALPHA};
			case Flags::Multiply:		return {GL_ZERO, GL_SRC_COLOR};
			default:					return {GL_INVALID_ENUM, GL_INVALID_ENUM};
		}
	}

	////////////////////////////////////////////////////////////////

	enum {
		BufferType_Geometry,
		BufferType_Indices,
		BufferType_Uniform,

		BufferType_Count,
	};
	
	using UniformUpdateFunction				= void(GLint location, const void* data);

	struct TextureState {
		GLuint								handle;
		GLuint								fbo;
		u16									width;
		u16									height;
		GLenum								internal_format;
		GLenum								format;
		GLenum								type;
	};

	struct State {
		enum : u32 {
			ShaderIDBits					= 8,
			RenderTargetIDBits				= 8,
			
			MaxNumShaders					= 1 << ShaderIDBits,
			MaxNumRenderTargets				= 1 << RenderTargetIDBits,

			ShiftShaderID					= Gfx::Shader::NumStateBits,
			MaskShaderState					= Gfx::Shader::MaskState,
			MaskShaderID					= (MaxNumShaders - 1) << ShiftShaderID,
			MaskShaderIDAndState			= (1 << (ShiftShaderID + ShaderIDBits)) - 1,

			ShiftRenderTargetID				= ShiftShaderID + ShaderIDBits,
			MaskRenderTargetID				= (MaxNumRenderTargets - 1) << ShiftRenderTargetID,

			/* texture state is currently not included in shadowed state */
			TextureIDBits					= 8,
			MaxNumTextures					= 1 << TextureIDBits,
		};
		
		u32									current_bits;
		u16									GetShader() { return (current_bits & MaskShaderID) >> ShiftShaderID; }
		u16									GetRenderTarget() { return (current_bits & MaskRenderTargetID) >> ShiftRenderTargetID; }

		GLuint								buffers[BufferType_Count];

		u16									num_uniforms;
		u16									num_shaders;
		u16									num_textures;

		const void* const*					uniform_values;
		const Gfx::Uniform::Type*			uniform_types;
		u8*									uniform_tex_unit;
		const char*							uniform_names;

		const Gfx::Shader::Flags*			shader_flags;
		GLint								shader_uniforms[MaxNumShaders];
		GLuint								shader_programs[MaxNumShaders];

		TextureState						texture_state[MaxNumTextures];
		const Gfx::Texture::Descriptor*		texture_descriptors;

	} g_state;

	void SetState(u32 bits, u32 force_change = 0);
	void ChangeState(u32 bits, u32 mask);

	void TrimLog(char* buf, int lines = 8);
}

////////////////////////////////////////////////////////////////

FORCEINLINE void* CreateSystemRenderer(Sys::Window* window) {
	using namespace GL;
	
	void* context = CreateGLContext(window);

	if (!CheckVersion()) {
		Sys::DebugLog("ERROR: OpenGL version check failed.\n");
		Sys::Fatal(Error::RenderVersion);
	}

	if (!LoadFunctions())
		Sys::Fatal(Error::RenderFunctions);

	g_state.current_bits = State::MaskRenderTargetID | State::MaskShaderID;

	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glEnable(GL_BLEND);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);

	glGenBuffers(BufferType_Count, g_state.buffers);

	return context;
}

////////////////////////////////////////////////////////////////

void GL::TrimLog(char* buf, int lines) {
	for (char* p = buf; *p; ++p) {
		if (*p == '\n' && --lines <= 0) {
			*p = 0;
			break;
		}
	}
}

FORCEINLINE void Gfx::RegisterShaders(const char* names, const Shader::Flags* flags, u16 count, const char* vertex_shaders, const char* fragment_shaders) {
	using namespace GL;

	g_state.shader_flags		= flags;
	g_state.num_shaders			= count;

	// Two passes, to enable parallel shader compilation

	const char* shader_name = names;
	for (u16 shader_index = 0; shader_index<count; ++shader_index, shader_name = NextAfter(shader_name)) {
		auto& program = g_state.shader_programs[shader_index];
		program = glCreateProgram();
		if (!program)
			Sys::Fatal(Error::Shader);

		const char*			sources			[] = {vertex_shaders, fragment_shaders};
		const GLenum		gl_stage_enums	[] = {GL_VERTEX_SHADER, GL_FRAGMENT_SHADER};
		const char* const	gl_stage_names	[] = {"Vertex", "Fragment"};
		int					shaders			[] = {0, 0};
		
		const u32		NumStages		= size(gl_stage_enums);

		for (u32 i=0; i<NumStages; ++i) {
			int stage_enum = gl_stage_enums[i];
			auto& shader = shaders[i];
			shader = glCreateShader(stage_enum);
			if (!shader)
				Sys::Fatal(Error::Shader);

			const char* src[] = {
				"#version 330\n",
				sources[i],
				"void main(){", shader_name, "();}\n",
			};

			glShaderSource(shader, size(src), src, nullptr);
			glCompileShader(shader);

#ifdef DEV
			GLint is_compiled = 0;
			glGetShaderiv(shader, GL_COMPILE_STATUS, &is_compiled);
			if (!is_compiled) {
				GLint max_length = 0;
				glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &max_length);
				char* buf = Mem::Alloc<char>(max_length);
				glGetShaderInfoLog(shader, max_length, &max_length, buf);
				auto stage_name = gl_stage_names[i];
				Sys::DebugStream << stage_name << " shader compilation failed (" << shader_name << "):\n" << buf << "\n";
				TrimLog(buf, 8);
				MessageBoxA(0, buf, "Shader compilation failed", MB_OK);
				Sys::Fatal(Error::Shader);
			}
#endif

			glAttachShader(program, shader);
		}

#if 0
		{
			#define PP_ADD_GFX_ATTRIB_SHADER_NAME(name, type)	"v" ## #name ## "\0"
			static constexpr char AttributeNames[] = GFX_VERTEX_ATTRIBUTES(PP_ADD_GFX_ATTRIB_SHADER_NAME) "\0";
			#undef PP_ADD_GFX_ATTRIB_SHADER_NAME

			GLuint index = 0;
			for (const char* attrib_name = AttributeNames; index < (u8)Vertex::Attribute::Count; ++index, attrib_name = NextAfter(attrib_name))
				glBindAttribLocation(program, index, attrib_name);
		}
#endif
		glLinkProgram(program);

#ifdef DEV
		for (u32 i=0; i<NumStages; ++i) {
			glDetachShader(program, shaders[i]);
			glDeleteShader(shaders[i]);
		}
#endif
	}

	auto uniforms = g_state.shader_uniforms;
	for (u16 shader_index = 0; shader_index<count; ++shader_index, uniforms += g_state.num_uniforms) {
		auto& program = g_state.shader_programs[shader_index];

		GLint is_linked = 0;
		glGetProgramiv(program, GL_LINK_STATUS, &is_linked);
		if (!is_linked) {
#ifdef DEV
			GLint max_length = 0;
			glGetProgramiv(program, GL_INFO_LOG_LENGTH, &max_length);
			char* buf = Mem::Alloc<char>(max_length);
			glGetProgramInfoLog(program, max_length, &max_length, buf);
			Sys::DebugStream << "Program linking failed:\n" << buf << "\n";
			TrimLog(buf, 8);
			MessageBoxA(0, buf, "Program linking failed", MB_OK);
#endif
			Sys::Fatal(Error::Shader);
		}

		glUseProgram(program);

		const char* uniform_name = g_state.uniform_names;
		for (u16 uniform_index = 0; uniform_index < g_state.num_uniforms; ++uniform_index, uniform_name = NextAfter(uniform_name)) {
			auto location = glGetUniformLocation(program, uniform_name);
			uniforms[uniform_index] = location;
			if (location != -1 && g_state.uniform_types[uniform_index] == Uniform::Type::Sampler)
				glUniform1i(location, g_state.uniform_tex_unit[uniform_index]);
		}
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Gfx::RegisterTextures(const Texture::Descriptor* textures, u16 count) {
	using namespace GL;	

	g_state.texture_descriptors	= textures;
	g_state.num_textures		= count;

	for (u16 texture_index = 0; texture_index < count; ++texture_index) {
		auto& descriptor = textures[texture_index];
		auto& texture = g_state.texture_state[texture_index];

		texture.handle			= 0;
		texture.fbo				= 0;
		texture.width			= descriptor.width  > 0 ? descriptor.width  : Sys::g_window.width;
		texture.height			= descriptor.height > 0 ? descriptor.height : Sys::g_window.height;
		texture.internal_format	= GetInternalFormat(descriptor.format);
		texture.format			= GetFormat(descriptor.format);
		texture.type			= GetType(descriptor.format);

		glGenTextures(1, &texture.handle);
		glBindTexture(GL_TEXTURE_2D, texture.handle);
		
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		if (descriptor.flags & Texture::Flags::NoMips) {
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		} else {
			glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		}

		glTexImage2D(GL_TEXTURE_2D, 0, texture.internal_format, texture.width, texture.height, 0, texture.format, texture.type, nullptr);

		if (descriptor.flags & Texture::Flags::RenderTarget) {
			glGenFramebuffers(1, &texture.fbo);
			glBindFramebuffer(GL_FRAMEBUFFER, texture.fbo);
			glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture.handle, 0);

			if (descriptor.flags & Texture::Flags::ZBuffer) {
				i16 zbuffer = -1;
				for (i16 i = texture_index; i >= 0; --i) {
					using namespace Texture;
					if ((textures[i].flags & (Flags::RenderTarget|Flags::ZBuffer)) == Flags::ZBuffer) {
						zbuffer = i;
						break;
					}
				}
				assert(zbuffer != -1);
				glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, g_state.texture_state[zbuffer].handle, 0);
			}
#ifdef DEV
			GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
			if (status != GL_FRAMEBUFFER_COMPLETE) {
				Sys::DebugStream << "Failed to create framebuffer.\n";
				Sys::Fatal(Error::RenderTarget);
			}
#endif
		}
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);
	glBindTexture(GL_TEXTURE_2D, 0);
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Gfx::RegisterUniforms(const char* names, const Uniform::Type* types, const void* const* values, u16 count) {
	using namespace GL;

	g_state.uniform_values		= values;
	g_state.uniform_types		= types;
	g_state.uniform_tex_unit	= Mem::Alloc<u8>(count);
	g_state.uniform_names		= names;
	g_state.num_uniforms		= count;

	u8 texture_unit = 0;
	for (u16 uniform_index = 0; uniform_index < count; ++uniform_index)
		if (types[uniform_index] == Uniform::Type::Sampler)
			g_state.uniform_tex_unit[uniform_index] = texture_unit++;
}

////////////////////////////////////////////////////////////////

void Gfx::SetTextureContents(Texture::ID id, const void* pixels, const IRect* rect) {
	using namespace GL;

	auto& texture = g_state.texture_state[id];
	IRect full = {0, 0, texture.width, texture.height};
	if (!rect)
		rect = &full;
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glTexSubImage2D(GL_TEXTURE_2D, 0, rect->x, rect->y, rect->w, rect->h, texture.format, texture.type, pixels);
}

void Gfx::GenerateMipMaps(Texture::ID id) {
	using namespace GL;

	auto& texture = g_state.texture_state[id];
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glGenerateMipmap(GL_TEXTURE_2D);
}

void Gfx::ReadBack(Texture::ID id, void* pixels) {
	using namespace GL;

	auto& texture = g_state.texture_state[id];
	glBindTexture(GL_TEXTURE_2D, texture.handle);
	glGetTexImage(GL_TEXTURE_2D, 0, texture.format, texture.type, pixels);
}

////////////////////////////////////////////////////////////////

NOINLINE void GL::SetState(u32 bits, u32 force_change) {
	using namespace Gfx;

	u32 changed_bits = (bits ^ g_state.current_bits) | force_change;

	if (changed_bits & GL::State::MaskRenderTargetID) {
		auto id = (bits & GL::State::MaskRenderTargetID) >> GL::State::ShiftRenderTargetID;
		auto fbo = id < g_state.num_textures ? g_state.texture_state[id].fbo : 0;
		glBindFramebuffer(GL_FRAMEBUFFER, fbo);
		GLsizei width, height;
		if (fbo) {
			width = g_state.texture_state[id].width;
			height = g_state.texture_state[id].height;
		} else {
			width = Sys::g_window.width;
			height = Sys::g_window.height;
		}
		glViewport(0, 0, width, height);
	}

	if (changed_bits & GL::State::MaskShaderID) {
		auto id = (bits & GL::State::MaskShaderID) >> GL::State::ShiftShaderID;
		glUseProgram(g_state.shader_programs[id]);
	}

	if (changed_bits & Shader::Flags::MaskAttribs) {
		for (u8 attrib = 0; attrib < Vertex::MaxNumAttributes; ++attrib) {
			auto current_bit = 1 << attrib;
			if (!(changed_bits & current_bit))
				continue;
			if (bits & current_bit)
				glEnableVertexAttribArray(attrib);
			else
				glDisableVertexAttribArray(attrib);
		}
	}

	if (changed_bits & Shader::Flags::MaskBlend) {
		auto blend_factor = GetBlendFactors(Shader::Flags(bits));
		glBlendFunc(blend_factor.src, blend_factor.dst);
	}

	if (changed_bits & Shader::Flags::MaskZWrite) {
		bool write = !(bits & Shader::Flags::NoZWrite);
		glDepthMask(write);
	}
	
	if (changed_bits & Shader::Flags::MaskCull) {
		bool cull = !(bits & Shader::Flags::NoCull);
		if (cull)
			glEnable(GL_CULL_FACE);
		else
			glDisable(GL_CULL_FACE);
	}
	
	if (changed_bits & Shader::Flags::MaskZTest) {
		GLenum depth_func = GL_LESS;
		switch (bits & Shader::Flags::MaskZTest) {
			case Shader::Flags::ZTestAlways:
				depth_func = GL_ALWAYS;
				break;

			case Shader::Flags::ZTestLess:
			default:
				break;
		}
		glDepthFunc(depth_func);
	}

	g_state.current_bits = bits;
}

FORCEINLINE void GL::ChangeState(u32 bits, u32 mask) {
	SetState(SelectBits(mask, bits, g_state.current_bits));
}

FORCEINLINE void Gfx::SetShader(Shader::ID id) {
	using namespace GL;
	u32 bits = (id << GL::State::ShiftShaderID) | g_state.shader_flags[id];
	GL::ChangeState(bits, State::MaskShaderIDAndState);
}

FORCEINLINE void Gfx::SetRenderTarget(Texture::ID id) {
	using namespace GL;
	u32 bits = id << GL::State::ShiftRenderTargetID;
	GL::ChangeState(bits, State::MaskRenderTargetID);
}

////////////////////////////////////////////////////////////////

NOINLINE void Gfx::UpdateUniforms() {
	using namespace GL;

	u16 shader = g_state.GetShader();
	GLint* locations = g_state.shader_uniforms + shader * g_state.num_uniforms;
	
	for (u16 uniform = 0; uniform < g_state.num_uniforms; ++uniform) {
		GLint location = locations[uniform];
		if (location == -1)
			continue;

		const void* data = g_state.uniform_values[uniform];
		switch (g_state.uniform_types[uniform]) {
			case Uniform::Type::Vec4:
				glUniform4fv(location, 1, (const float*)data);
				break;

			case Uniform::Type::Mat4:
				glUniformMatrix4fv(location, 1, GL_FALSE, (const float*)data);
				break;

			case Uniform::Type::Sampler: {
				auto id = *(const Gfx::Texture::ID*)data;
				GLuint handle = id < g_state.num_textures ? g_state.texture_state[id].handle : 0;
				glActiveTexture(GL_TEXTURE0 + g_state.uniform_tex_unit[uniform]);
				glBindTexture(GL_TEXTURE_2D, handle);
				break;
			}

			default:
				break;
		}
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Gfx::Clear(u16 mask, const vec4& color, float z) {
	using namespace GL;
	GLbitfield bits = 0;

	if (mask & ClearBit::Color) {
		glClearColor(color.r, color.g, color.b, color.a);
		bits |= GL_COLOR_BUFFER_BIT;
	}

	if (mask & ClearBit::Depth) {
		SetState(g_state.current_bits & ~Shader::NoZWrite);
		glClearDepth(z);
		bits |= GL_DEPTH_BUFFER_BIT;
	}

	if (bits)
		glClear(bits);
}

////////////////////////////////////////////////////////////////

namespace GL {
	template <GLint Size, GLint Type>
	struct BaseTypeInfo { enum { size = Size, type = Type }; };
	
	template <typename T> struct TypeInfo;
	template <> struct TypeInfo <vec2> : BaseTypeInfo <2, GL_FLOAT>{};
	template <> struct TypeInfo <vec3> : BaseTypeInfo <3, GL_FLOAT>{};
	template <> struct TypeInfo <vec4> : BaseTypeInfo <4, GL_FLOAT>{};
	template <> struct TypeInfo <u32 > : BaseTypeInfo <4, GL_UNSIGNED_BYTE>{};

	static constexpr u8 AttribSizes[] = {
		#define PP_GFX_ADD_ATTRIB_SIZE(name, type)	GL::TypeInfo<type>::size,
		GFX_VERTEX_ATTRIB_TYPES(PP_GFX_ADD_ATTRIB_SIZE)
		#undef PP_GFX_ADD_ATTRIB_SIZE
	};

	static constexpr GLenum AttribTypes[] = {
		#define PP_GFX_ADD_ATTRIB_TYPE(name, t)		GL::TypeInfo<t>::type,
		GFX_VERTEX_ATTRIB_TYPES(PP_GFX_ADD_ATTRIB_TYPE)
		#undef PP_GFX_ADD_ATTRIB_TYPE
	};
}

////////////////////////////////////////////////////////////////

NOINLINE void Gfx::Draw(const Mesh& mesh) {
	using namespace GL;

	auto shader = g_state.GetShader();

	u32 bits = (shader << GL::State::ShiftShaderID) | g_state.shader_flags[shader];
	GL::ChangeState(g_state.shader_flags[shader], GL::State::MaskShaderState);

	auto flags = g_state.shader_flags[shader];
	for (u8 attrib = 0; attrib < Vertex::MaxNumAttributes; ++attrib) {
		if (flags & (1 << attrib)) {
			auto& stream = mesh.vertices[attrib];
			if (stream.data) {
				glVertexAttribPointer(attrib, GL::AttribSizes[(u8)stream.type], GL::AttribTypes[(u8)stream.type], stream.normalized, stream.stride, stream.data);
			} else {
				Sys::DebugStream << "Invalid draw call: missing mesh stream\n";
				Sys::Breakpoint();
				return;
			}
		}
	}

	if (mesh.num_indices) {
		glDrawElements(GL_TRIANGLES, mesh.num_indices, GL_UNSIGNED_SHORT, mesh.indices);
	} else {
		glDrawArrays(GL_TRIANGLES, 0, mesh.num_vertices);
	}
}

void Gfx::DrawFullScreen() {
	static constexpr vec3 positions[3] = {
		{-1.f, -1.f, -1.f},
		{ 3.f, -1.f, -1.f},
		{-1.f,  3.f, -1.f},
	};
	
	Gfx::Mesh mesh;
	memset(&mesh, 0, sizeof(mesh));
	mesh.num_vertices = 3;
	mesh.vertices[0].SetData(positions);

	Draw(mesh);
}

FORCEINLINE void Gfx::Sync() {
	glFinish();
}

FORCEINLINE vec2 Gfx::GetResolution() {
	return {(float)Sys::g_window.width, (float)Sys::g_window.height};
}