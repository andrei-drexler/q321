#pragma once

////////////////////////////////////////////////////////////////

#ifndef ENABLE_RENDERDOC
	#ifdef DEV
		#define ENABLE_RENDERDOC
	#endif
#endif

////////////////////////////////////////////////////////////////

namespace Gfx {
	using ID = u16;
	
	static const ID
		InvalidID	= ID(-1),
		Backbuffer	= InvalidID;

	namespace Texture {
		using ID = Gfx::ID;

		enum class Format : i8 {
			BGRA8,
			RGBA16F,
			RGBA32F,
			Z32F,

			Count,
		};

		enum Flags : u8 {
			Default			= 0,
			NoMips			= 1 << 0,
			RenderTarget	= 1 << 1,
			ZBuffer			= 1 << 2,
		};

		struct Descriptor {
			union {
				struct {
					u16		width;
					u16		height;
				};
				u16			size[2];
			};
			Format			format;
			Flags			flags;
		};
	}

	#define PP_GFX_TEXTURE_DESCRIPTOR(name, width, height, format, flags)		{ width, height, format, flags },
	#define PP_GFX_TEXTURE_ID(name, width, height, format, flags)				name,

	// Assigns shader ID's and creates static descriptor list
	#define GFX_DECLARE_TEXTURES(list)									\
		enum : Gfx::Texture::ID {										\
			list(PP_GFX_TEXTURE_ID)										\
			Count,														\
		};																\
		static constexpr Gfx::Texture::Descriptor Descriptors[] = {		\
			list(PP_GFX_TEXTURE_DESCRIPTOR)								\
		}																\

	////////////////////////////////////////////////////////////////

	namespace Uniform {
		#define GFX_UNIFORM_TYPES(x)		\
			x(Sampler,		Texture::ID)	\
			x(Vec4,			vec4)			\
			x(Mat4,			mat4)			\

		enum Type : u8 {
			#define PP_GFX_UNIFORM_TYPE_ENUM(name, type)	name,
			GFX_UNIFORM_TYPES(PP_GFX_UNIFORM_TYPE_ENUM)
			#undef PP_GFX_UNIFORM_TYPE_ENUM

			Count,
		};

		static constexpr u8 TypeSize[] = {
			#define PP_GFX_UNIFORM_TYPE_SIZE(name, type) sizeof(type),
			GFX_UNIFORM_TYPES(PP_GFX_UNIFORM_TYPE_SIZE)
			#undef PP_GFX_UNIFORM_TYPE_SIZE
		};

		template <typename T> struct TypeToEnumImpl;
		#define PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM(name, type)\
			template <> struct TypeToEnumImpl<type>	{ static constexpr Type value = Type::name; };
		GFX_UNIFORM_TYPES(PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM)
		#undef PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM

		template <typename T> struct TypeToEnumImpl<T&> : TypeToEnumImpl<T>{};

		template <typename T>
		static constexpr Type TypeToEnum = TypeToEnumImpl<T>::value;
	}

	#define PP_GFX_DECLARE_UNIFORM_VARIABLE(name, type)			type name;
	#define PP_GFX_UNIFORM_NAME(name, type)						#name "\0"
	#define PP_GFX_UNIFORM_ADDRESS(name, type)					&name,
	#define PP_GFX_UNIFORM_TYPE(name, type)						Gfx::Uniform::TypeToEnum<decltype(name)>,
	#define PP_GFX_UNIFORM_SIZE(name, type)						sizeof(type),

	#define GFX_DECLARE_UNIFORMS(list)																		\
		inline namespace Variables {																		\
			list(PP_GFX_DECLARE_UNIFORM_VARIABLE)															\
		}																									\
		namespace Metadata {																				\
			static constexpr Gfx::Uniform::Type Types[] = { list(PP_GFX_UNIFORM_TYPE) };					\
			static constexpr u8 Sizes[] = { list(PP_GFX_UNIFORM_SIZE) };									\
			static constexpr void* Addresses[] = { list(PP_GFX_UNIFORM_ADDRESS) };							\
			enum { Count = size(Addresses) };																\
		}																									\
		FORCEINLINE void RegisterAll() {																	\
			Gfx::RegisterUniforms(Metadata::Types, Metadata::Addresses, Metadata::Count);					\
		}																									\

	////////////////////////////////////////////////////////////////

	namespace Vertex {
		enum {
			MaxNumAttributes = 8,
		};

		#define GFX_VERTEX_ATTRIB_TYPES(x)	\
			x(Vec2,			vec2)			\
			x(Vec3,			vec3)			\
			x(Vec4,			vec4)			\
			x(U8x4,			u32)			\

		enum class AttribType : u8 {
			#define PP_GFX_ATTRIB_TYPE_ENUM(name, type)	name,
			GFX_VERTEX_ATTRIB_TYPES(PP_GFX_ATTRIB_TYPE_ENUM)
			#undef PP_GFX_ATTRIB_TYPE_ENUM

			Count,
		};

		template <typename T> struct TypeToEnumImpl;
		#define PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM(name, type)\
			template <> struct TypeToEnumImpl<type>	{ static constexpr AttribType value = AttribType::name; };
		GFX_VERTEX_ATTRIB_TYPES(PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM)
		#undef PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM

		template <typename T> struct TypeToEnumImpl<T&> : TypeToEnumImpl<T>{};
		template <typename T> struct TypeToEnumImpl<const T> : TypeToEnumImpl<T>{};

		template <typename T>
		static constexpr AttribType TypeToEnum = TypeToEnumImpl<T>::value;
	}

	////////////////////////////////////////////////////////////////

	namespace Shader {
		using ID = Gfx::ID;

		enum Flags {
			MaskAttribs			= (1 << Vertex::MaxNumAttributes) - 1,

			ShiftBlend			= Vertex::MaxNumAttributes,
			Opaque				= 0 << ShiftBlend,
			Premultiplied		= 1 << ShiftBlend,
			Multiply			= 2 << ShiftBlend,
			MaskBlend			= 3 << ShiftBlend,
			NumBlendModes		= 3,

			NoZWrite			= 1 << (2 + Vertex::MaxNumAttributes),
			MaskZWrite			= 1 << (2 + Vertex::MaxNumAttributes),

			ZTestLess			= 0 << (3 + Vertex::MaxNumAttributes),
			ZTestAlways			= 1 << (3 + Vertex::MaxNumAttributes),
			MaskZTest			= 1 << (3 + Vertex::MaxNumAttributes),

			NoCull				= 1 << (4 + Vertex::MaxNumAttributes),
			MaskCull			= 1 << (4 + Vertex::MaxNumAttributes),

			NumStateBits		= 5 + Vertex::MaxNumAttributes,
			MaskState			= (1 << NumStateBits) - 1,
		};

		struct Module {
#ifdef DISABLE_SHADER_STITCHING
			const char*			code;
#else
			u32					num_sections;
			const char*			code;
			const u32*			section_sizes;
			const u32*			shader_deps;
#endif
		};
	}

	#define PP_GFX_SHADER_ID(name, flags)		name,
	#define PP_GFX_SHADER_NAME(name, flags)		#name "\0"
	#define PP_GFX_SHADER_FLAGS(name, flags)	(Gfx::Shader::Flags)(flags),

	#define GFX_DECLARE_SHADERS(list)											\
		enum : Gfx::Shader::ID {												\
			list(PP_GFX_SHADER_ID)												\
			Count,																\
		};																		\
		namespace Metadata {													\
			static constexpr char Names[] =	list(PP_GFX_SHADER_NAME);			\
			static constexpr Gfx::Shader::Flags Properties[] = {				\
				list(PP_GFX_SHADER_FLAGS)										\
			};																	\
		}																		\
		FORCEINLINE void RegisterAll(const Gfx::Shader::Module* modules) {		\
			using namespace Metadata;											\
			Gfx::RegisterShaders(Count, Properties, modules[0], modules[1]);	\
		}																		\

	////////////////////////////////////////////////////////////////

	namespace Clear {
		namespace Mask {
			enum {
				Color			= 1 << 0,
				Depth			= 1 << 1,
				ColorAndDepth	= Color | Depth,
			};
		}

		struct Mode {
			u32				what;
			vec4			color;
			float			depth;
		};

		static constexpr Mode
			Color			{Mask::Color,         {0.f, 0.f, 0.f, 0.f}, 0.f},
			Depth			{Mask::Depth,         {0.f, 0.f, 0.f, 0.f}, 1.f},
			ColorAndDepth	{Mask::ColorAndDepth, {0.f, 0.f, 0.f, 0.f}, 1.f}
		;
	};

	////////////////////////////////////////////////////////////////

	namespace Arena {
		enum Type {
			Permanent,
			Level,
			Dynamic,

			Count,
		};

		// offset added to all allocations so that we can use 0 as NULL
		constexpr u32 BaseOffset = 4096;
	};

	void InitMemory(const u32 sizes[Arena::Count]);
	void ResetArena(Arena::Type type);
	u32 UploadGeometry(const void* data, u32 size, Arena::Type type = Arena::Type::Dynamic);
	void UploadGeometry(const void* data, u32 size, Arena::Type type, u32 offset);

	template <typename T>
	u32 UploadGeometry(const T* data, u32 count, Arena::Type type = Arena::Type::Dynamic) {
		return UploadGeometry((const void*)data, count * sizeof(T), type);
	}

	template <typename T>
	void UploadGeometry(const T* data, u32 count, Arena::Type type, u32 offset) {
		UploadGeometry((const void*)data, count * sizeof(T), type, offset);
	}

	void RegisterUniforms(const Uniform::Type* types, const void* const* values, u16 count);
	void RegisterTextures(const Texture::Descriptor* textures, u16 count);
	void RegisterShaders(u16 count, const Shader::Flags* flags, const Shader::Module& vertex_shaders, const Shader::Module& fragment_shaders);
	void CompileShaders(Shader::ID first, u16 count);

	template <int Size>
	void FORCEINLINE RegisterTextures(const Texture::Descriptor (&descriptors)[Size]) {
		RegisterTextures(descriptors, Size);
	}

	////////////////////////////////////////////////////////////////

	struct Mesh;

	void SetTextureContents(Texture::ID id, const void* pixels, const IRect* rect = nullptr);
	void GenerateMipMaps(Texture::ID id);
	void ReadBack(Texture::ID id, void* pixels);
	const Texture::Descriptor& GetTextureDescriptor(Texture::ID id);
	bool SaveTGA(const char* path, const u32* pixels, u16 width, u16 height);
	bool SaveTGA(const char* path, Texture::ID id);

	void SetRenderTarget(Texture::ID id, const Clear::Mode* clear = nullptr, const IRect* viewport = nullptr);
	void SetShader(Shader::ID id);
	void UpdateUniforms();

	void DrawFullScreen();
	void Draw(const Mesh& mesh);

	void Present();
	void Sync();

	vec2 GetResolution();
	float GetAspectRatio();

	////////////////////////////////////////////////////////////////

	struct Mesh {
		struct VertexStream {
			u32					addr;
			Vertex::AttribType	type;
			bool				normalized;
			u8					stride;

			VertexStream() = default;
			VertexStream(const VertexStream&) = default;

			constexpr VertexStream(u32 addr, Vertex::AttribType type, bool normalized, u8 stride) :
				addr(addr),
				type(type),
				normalized(normalized),
				stride(stride)
			{ }

			template <typename T>
			FORCEINLINE VertexStream& SetData(const T* contents, u32 count) {
				addr = Gfx::UploadGeometry(contents, count);
				type = Gfx::Vertex::TypeToEnum<T>;
				return *this;
			}

			template <typename T>
			FORCEINLINE VertexStream& SetData(u32 contents, u32 offset = 0) {
				addr = contents + offset * sizeof(T);
				type = Gfx::Vertex::TypeToEnum<T>;
				return *this;
			}

			template <typename T>
			FORCEINLINE constexpr VertexStream& SetType() {
				type = Gfx::Vertex::TypeToEnum<T>;
				return *this;
			}
		}						vertices[Vertex::MaxNumAttributes];
		u32						index_addr;

		Mesh& SetIndices(const u32* indices, u32 count) {
			index_addr = Gfx::UploadGeometry(indices, count);
			num_indices = count;
			return *this;
		}

		u32						num_vertices;
		u32						num_indices;
	};

	////////////////////////////////////////////////////////////////

	namespace TGA {
		#pragma pack(push, 1)
		struct Header {
			u8		identSize;
			u8		colorMapType;
			u8		imageType;
			u16		colorMapStart;
			u16		colorMapLength;
			u8		colorMapBits;
			u16		originX;
			u16		originY;
			u16		width;
			u16		height;
			u8		colorBits;
			u8		imageInfo;
		};
		#pragma pack(pop)

		enum Format {
			Uncompressed = 2,
			CompressedRLE = 10,
		};
	}
}

////////////////////////////////////////////////////////////////

// forceinline, assuming it's only called from one place (for screenshots)
FORCEINLINE bool Gfx::SaveTGA(const char* path, const u32* pixels, u16 width, u16 height) {
	Sys::File::Handle file = Sys::OpenFile(path, Sys::File::Write);
	if (!file)
		return false;

	TGA::Header header;
	MemSet(&header);

	header.colorBits = 32;
	header.imageType = TGA::Uncompressed;
	header.width = width;
	header.height = height;

	bool result;
	result = Sys::WriteToFile(file, &header, sizeof(header));
	result |= Sys::WriteToFile(file, pixels, width * height * sizeof(u32));

	Sys::CloseFile(file);

	return result;
}

bool Gfx::SaveTGA(const char* path, Texture::ID id) {
	const Texture::Descriptor& descriptor = GetTextureDescriptor(id);
	u32* pixels = Sys::Alloc<u32>(descriptor.width * descriptor.height);
	Gfx::ReadBack(id, pixels);
	bool result = Gfx::SaveTGA(path, pixels, descriptor.width, descriptor.height);
	Sys::Free(pixels);
	return result;
}
