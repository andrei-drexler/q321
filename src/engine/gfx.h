#pragma once

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
			u16				width;
			u16				height;
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

		template <typename T> struct TypeToEnum;
		#define PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM(name, type)\
			template <> struct TypeToEnum<type>	{ static constexpr Type value = Type::name; };
		GFX_UNIFORM_TYPES(PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM)
		#undef PP_GFX_MAP_UNIFORM_TYPE_TO_ENUM

		template <typename T> struct TypeToEnum<T&> : TypeToEnum<T>{};
	}

	#define PP_GFX_DECLARE_UNIFORM_VARIABLE(name, type)			type name;
	#define PP_GFX_UNIFORM_NAME(name, type)						#name "\0"
	#define PP_GFX_UNIFORM_ADDRESS(name, type)					&name,
	#define PP_GFX_UNIFORM_TYPE(name, type)						Gfx::Uniform::TypeToEnum<decltype(name)>::value,
		
	#define GFX_DECLARE_UNIFORMS(list)														\
		inline namespace Variables {														\
			list(PP_GFX_DECLARE_UNIFORM_VARIABLE)											\
		}																					\
		namespace Metadata {																\
			static constexpr char Names[] =	list(PP_GFX_UNIFORM_NAME);						\
			static constexpr Gfx::Uniform::Type Types[] = { list(PP_GFX_UNIFORM_TYPE) };	\
			static constexpr void const* Addresses[] = { list(PP_GFX_UNIFORM_ADDRESS) };	\
			enum { Count = size(Addresses) };												\
		}																					\
		FORCEINLINE void RegisterAll() {													\
			using namespace Metadata;														\
			Gfx::RegisterUniforms(Names, Types, Addresses, Count);							\
		}																					\

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

		template <typename T> struct TypeToEnum;
		#define PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM(name, type)\
			template <> struct TypeToEnum<type>	{ static constexpr AttribType value = AttribType::name; };
		GFX_VERTEX_ATTRIB_TYPES(PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM)
		#undef PP_GFX_MAP_ATTRIB_TYPE_TO_ENUM

		template <typename T> struct TypeToEnum<T&> : TypeToEnum<T>{};
		template <typename T> struct TypeToEnum<const T> : TypeToEnum<T>{};
	}

	////////////////////////////////////////////////////////////////

	struct Mesh {
		struct VertexStream {
			const void*			data;
			Vertex::AttribType	type;
			bool				normalized;
			u8					stride;

			VertexStream() = default;

			template <typename T>
			constexpr VertexStream(const T* contents, bool normalized = true, u8 stride = 0) :
				data		(contents),
				type		(Gfx::Vertex::TypeToEnum<T>::value),
				normalized	(normalized),
				stride		(stride)
			{ }

			template <typename T>
			FORCEINLINE VertexStream& SetData(const T* contents) {
				data = contents;
				type = Gfx::Vertex::TypeToEnum<T>::value;
				return *this;
			}
		}						vertices[Vertex::MaxNumAttributes];
		const u16*				indices;

		u16						num_vertices;
		u16						num_indices;
	};

	////////////////////////////////////////////////////////////////

	namespace Shader {
		using ID = Gfx::ID;

		enum Flags {
			MaskAttribs			= (1 << Vertex::MaxNumAttributes) - 1,

			Opaque				= 0 << (0 + Vertex::MaxNumAttributes),
			Premultiplied		= 1 << (0 + Vertex::MaxNumAttributes),
			Multiply			= 2 << (0 + Vertex::MaxNumAttributes),
			MaskBlend			= 3 << (0 + Vertex::MaxNumAttributes),

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
	}

	#define PP_GFX_SHADER_ID(name, flags)		name,
	#define PP_GFX_SHADER_NAME(name, flags)		#name "\0"
	#define PP_GFX_SHADER_FLAGS(name, flags)	(Gfx::Shader::Flags)(flags),

	#define GFX_DECLARE_SHADERS(list)																\
		enum : Gfx::Shader::ID {																	\
			list(PP_GFX_SHADER_ID)																	\
			Count,																					\
		};																							\
		namespace Metadata {																		\
			static constexpr char Names[] =	list(PP_GFX_SHADER_NAME);								\
			static constexpr Gfx::Shader::Flags Properties[] = {									\
				list(PP_GFX_SHADER_FLAGS)															\
			};																						\
		}																							\
		FORCEINLINE void RegisterAll(const char* vertex_shaders, const char* fragment_shaders) {	\
			using namespace Metadata;																\
			Gfx::RegisterShaders(Names, Properties, Count, vertex_shaders, fragment_shaders);		\
		}																							\

	////////////////////////////////////////////////////////////////

	namespace ClearBit {
		enum {
			Color			= 1 << 0,
			Depth			= 1 << 1,
			ColorAndDepth	= Color | Depth,
		};
	}

	////////////////////////////////////////////////////////////////

	void RegisterUniforms(const char* names, const Uniform::Type* types, const void* const* values, u16 count);
	void RegisterTextures(const Texture::Descriptor* textures, u16 count);
	void RegisterShaders(const char* names, const Shader::Flags* flags, u16 count, const char* vertex_shaders, const char* fragment_shaders);

	template <int Size>
	void FORCEINLINE RegisterTextures(const Texture::Descriptor (&descriptors)[Size]) {
		RegisterTextures(descriptors, Size);
	}

	////////////////////////////////////////////////////////////////

	void SetTextureContents(Texture::ID id, const void* pixels, const IRect* rect = nullptr);
	void GenerateMipMaps(Texture::ID id);
	void ReadBack(Texture::ID id, void* pixels);
	bool SaveTGA(const char* path, const u32* pixels, u16 width, u16 height);
	
	void SetRenderTarget(Texture::ID id, const IRect* viewport = nullptr);
	void SetShader(Shader::ID id);
	void UpdateUniforms();
	
	void Clear(u16 mask, const vec4& color = vec4(0.f), float z = 1.f);
	void DrawFullScreen();
	void Draw(const Mesh& mesh);
	
	void Sync();
	
	vec2 GetResolution();

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

bool Gfx::SaveTGA(const char* path, const u32* pixels, u16 width, u16 height) {
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
