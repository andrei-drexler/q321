#pragma once

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

	////////////////////////////////////////////////////////////////

	struct Triangle {
		u32					indices[3];
	};

	////////////////////////////////////////////////////////////////

	using UV = vec2;

	////////////////////////////////////////////////////////////////

	struct Vertex {
		static const u16 FracBits = 6;
		static const i16 Scale = 1 << FracBits;

		i16					pos[3];
		i16					nor;

		vec3				GetPosition() const { return vec3(pos[0], pos[1], pos[2]) / float(Scale); }
	};

	////////////////////////////////////////////////////////////////

	struct Shader {
		String				name;
		u32					index;
	};

	////////////////////////////////////////////////////////////////

	struct Tag {
		String				name;
		vec3				origin;
		vec3				axis[3];
	};

	////////////////////////////////////////////////////////////////

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
		u32*				GetIndices()			{ return GetPtr<u32>(this, ofs_tris); }
		const u32*			GetIndices() const		{ return GetPtr<u32>(this, ofs_tris); }
		Shader*				GetShaders()			{ return GetPtr<Shader>(this, ofs_shaders); }
		const Shader*		GetShaders() const		{ return GetPtr<Shader>(this, ofs_shaders); }
		UV*					GetUVs()				{ return GetPtr<UV>(this, ofs_uvs); }
		const UV*			GetUVs() const			{ return GetPtr<UV>(this, ofs_uvs); }
		Vertex*				GetVerts()				{ return GetPtr<Vertex>(this, ofs_verts); }
		const Vertex*		GetVerts() const		{ return GetPtr<Vertex>(this, ofs_verts); }
		
		Surface*			GetNext()				{ return GetPtr<Surface>(this, ofs_end); }
		const Surface*		GetNext() const			{ return GetPtr<Surface>(this, ofs_end); }
	};

	////////////////////////////////////////////////////////////////

	struct Header {
		static const u32 ExpectedIdent = FourCC("IDP3");
		static const u32 ExpectedVersion = 15;

		u32					ident;
		u32					version;
		String				name;
		u32					flags;

		u32					num_frames;
		u32					num_tags;
		u32					num_surfaces;
		u32					num_skins;

		u32					ofs_frames;
		u32					ofs_tags;
		u32					ofs_surfaces;

		u32					ofs_end;

		Frame*				GetFrames()				{ return GetPtr<Frame>(this, ofs_frames); }
		const Frame*		GetFrames() const		{ return GetPtr<Frame>(this, ofs_frames); }
		Surface*			GetFirstSurface()		{ return GetPtr<Surface>(this, ofs_surfaces); }
		const Surface*		GetFirstSurface() const	{ return GetPtr<Surface>(this, ofs_surfaces); }
		Tag*				GetTags()				{ return GetPtr<Tag>(this, ofs_tags); }
		const Tag*			GetTags() const			{ return GetPtr<Tag>(this, ofs_tags); }
	};

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

		const MD3::Surface* surf = model.GetFirstSurface();
		for (u32 i = 0; i < model.num_surfaces; ++i, surf = surf->GetNext()) {
			if (surf->num_shaders < 0) {
				return false;
			}
		}

		return true;
	}

	////////////////////////////////////////////////////////////////

	struct Model {
		std::vector<char> contents;

		bool Load(const char* path) {
			if (!ReadFile(path, contents, ReadMode::Silent))
				return false;
			return CheckModel(GetHeader());
		}

		Header&			GetHeader() { return *reinterpret_cast<Header*>(contents.data()); }
		const Header&	GetHeader() const { return *reinterpret_cast<const Header*>(contents.data()); }
	};

	////////////////////////////////////////////////////////////////
	// Helpers /////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////

	namespace Details {
		struct SurfaceRangeEnd { };

		struct ConstSurfaceIterator {
			const Surface*			surface;
			u32						remaining;

			const Surface&			operator*() const { return *surface; }
			const Surface*			operator->() const { return surface; }

			ConstSurfaceIterator&	operator++() { surface = surface->GetNext(); --remaining; return *this; }
			bool					operator!=(SurfaceRangeEnd) { return remaining; }
		};

		struct SurfaceIterator : ConstSurfaceIterator {
			Surface*				operator->() { return const_cast<Surface*>(surface); }
			Surface&				operator*() { return const_cast<Surface&>(*surface); }
		};

		struct ConstSurfaceList {
			const Header*			header;

			ConstSurfaceIterator	begin() { return {header->GetFirstSurface(), header->num_surfaces}; }
			SurfaceRangeEnd			end() { return {}; }
		};

		struct SurfaceList {
			Header*					header;

			SurfaceIterator			begin() { return {header->GetFirstSurface(), header->num_surfaces}; }
			SurfaceRangeEnd			end() { return {}; }
		};

		////////////////////////////////////////////////////////////////

		struct PositionHasher {
			inline size_t operator()(const MD3::Vertex& v) const {
				size_t result = HashValue(v.pos[0]);
				result = HashCombine(result, v.pos[1]);
				result = HashCombine(result, v.pos[2]);
				return result;
			}
		};

		struct PositionEqualityComparator {
			inline bool operator()(const MD3::Vertex& a, const MD3::Vertex& b) const {
				return memcmp(a.pos, b.pos, sizeof(a.pos)) == 0;
			}
		};
	} // namespace Details

	////////////////////////////////////////////////////////////////

	Details::SurfaceList			GetSurfaces(Header& header) { return {&header}; }
	Details::ConstSurfaceList		GetSurfaces(const Header& header) { return {&header}; }

	array_view<Vertex>				GetVertices(Surface& surface) { return {surface.GetVerts(), surface.num_verts}; }
	array_view<const Vertex>		GetVertices(const Surface& surface) { return {surface.GetVerts(), surface.num_verts}; }

	array_view<UV>					GetUVs(Surface& surface) { return {surface.GetUVs(), surface.num_verts}; }
	array_view<const UV>			GetUVs(const Surface& surface) { return {surface.GetUVs(), surface.num_verts}; }

	array_view<Triangle>			GetTriangles(Surface& surface) { return {surface.GetTris(), surface.num_tris}; }
	array_view<const Triangle>		GetTriangles(const Surface& surface) { return {surface.GetTris(), surface.num_tris}; }

	array_view<u32>					GetIndices(Surface& surface) { return {surface.GetIndices(), surface.num_tris * 3}; }
	array_view<const u32>			GetIndices(const Surface& surface) { return {surface.GetIndices(), surface.num_tris * 3}; }

	array_view<Frame>				GetFrames(Header& header) { return {header.GetFrames(), header.num_frames}; }
	array_view<const Frame>			GetFrames(const Header& header) { return {header.GetFrames(), header.num_frames}; }

	array_view<Tag>					GetTags(Header& header) { return {header.GetTags(), header.num_tags}; }
	array_view<const Tag>			GetTags(const Header& header) { return {header.GetTags(), header.num_tags}; }

	////////////////////////////////////////////////////////////////

	template <typename Value>
	using VertexPositionHashMap = std::unordered_map<Vertex, Value, Details::PositionHasher, Details::PositionEqualityComparator>;
}
