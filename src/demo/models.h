#pragma once

namespace Demo {
	struct PackedModel {
		struct Part {
			u8			material;
			u16			num_verts;
			u16			num_indices;
		};

		u8				num_parts;

		const Part*		parts;
		const u8*		verts; // separate X/Y/Z/S/T streams
		const u16*		stream_lengths;
		const u8*		indices; // varint encoded
	};

	////////////////////////////////////////////////////////////////

	namespace Model {
		static constexpr float
			PositionScale = 4.f,
			UVScale = 64.f
		;

		enum ID {
			#define PP_ADD_MODEL_ID(path, name,...) name,
			DEMO_MODELS(PP_ADD_MODEL_ID)
			#undef PP_ADD_MODEL_ID

			Count,
		};

		struct Transform {
			vec3				position;
			vec3				angles;
			float				scale;
		};

		static constexpr Transform TransformIdentity = {
			{0.f, 0.f, 0.f},	// position
			{0.f, 0.f, 0.f},	// angles
			1.f,				// scale
		};

		struct Part {
			u16					num_verts;
			u16					num_indices;
			u8					material;
			u32					ofs_verts;
			u32					ofs_idx;
		};

		struct Entry {
			u16					first_part;
			u8					num_part;
		};

		struct Vertex {
			vec3				pos;
			vec3				nor;
			vec2				uv;
		};

		namespace Storage {
			vec3*				vertices;
			vec2*				uvs;
			vec3*				normals;
			u32*				indices;
			Part*				parts;
			u16*				first_model_part;

			u32					num_verts;
			u32					num_indices;
			u32					num_parts;

			struct {
				u32				vertices;
				u32				uvs;
				u32				normals;
				u32				indices;
			}					gpu_addr;
		}

		Entry list[Model::Count];

		void LoadAll(const PackedModel* models);
		void Draw(Model::ID id, const Transform& transform = TransformIdentity);

		void PackNormalOffset(vec3& normal, const vec3& offset);
		void UnpackNormal(const vec3& posnor, vec3& nor);
	};
}

////////////////////////////////////////////////////////////////
// Implementation //////////////////////////////////////////////
////////////////////////////////////////////////////////////////

NOINLINE u32 ReadVarint(const u8*& ptr) {
	u32 result = 0;

	u32 shift = 0;
	u8 fragment;
	do {
		fragment = *ptr++;
		/* varint decoding */
		result |= (fragment & 127) << shift;
		shift += 7;
	} while (fragment >= 128);

	return result;
}

FORCEINLINE void Demo::Model::LoadAll(const PackedModel* models) {
	assert(!Storage::parts);

	/* count resources */
	for (u16 model_index = 0; model_index < Model::Count; ++model_index) {
		const PackedModel& packed = models[model_index];
		for (u16 part_index = 0; part_index < packed.num_parts; ++part_index) {
			Storage::num_indices += packed.parts[part_index].num_indices;
			Storage::num_verts += packed.parts[part_index].num_verts;
		}
		Storage::num_parts += packed.num_parts;
	}

	/* alloc resources */
	Storage::vertices         = Mem::Alloc< vec3 >(Storage::num_verts);
	Storage::uvs              = Mem::Alloc< vec2 >(Storage::num_verts);
	Storage::normals          = Mem::Alloc< vec3 >(Storage::num_verts);
	Storage::indices          = Mem::Alloc< u32  >(Storage::num_indices);
	Storage::parts            = Mem::Alloc< Part >(Storage::num_parts);
	Storage::first_model_part = Mem::Alloc< u16  >(Model::Count + 1);

	MemSet(Storage::normals, 0, Storage::num_verts);

	/* initialize resources */
	u32 dst_ofs_part  = 0;
	u32 dst_ofs_verts = 0;
	u32 dst_ofs_idx   = 0;

	for (u16 model_index = 0; model_index < Model::Count; ++model_index) {
		const PackedModel& packed = models[model_index];
		u32 num_vertices = 0;
		for (u16 part_index = 0; part_index < packed.num_parts; ++part_index)
			num_vertices += packed.parts[part_index].num_verts;

		const u8* src_idx = packed.indices;
		const u8* src_pos[5];
		const u8 *stream = packed.verts;
		for (u32 i = 0; i < 5; ++i) {
			src_pos[i] = stream;
			stream += packed.stream_lengths[i] + num_vertices;
		}

		/* model -> part association */
		Storage::first_model_part[model_index] = dst_ofs_part;

		/* part properties */
		for (u16 part_index = 0; part_index < packed.num_parts; ++part_index) {
			const PackedModel::Part& src_part = packed.parts[part_index];
			Part& dst_part = Storage::parts[dst_ofs_part++];

			dst_part.material    = src_part.material;
			dst_part.num_verts   = src_part.num_verts;
			dst_part.num_indices = src_part.num_indices;
			dst_part.ofs_verts   = dst_ofs_verts;
			dst_part.ofs_idx     = dst_ofs_idx;

			bool is_sprite = Material::Properties[src_part.material] & Material::Sprite;

#ifdef DEV
			const char* path = Material::Paths[src_part.material];
#endif

#if DEMO_MODELS_USE_DELTA_ENCODING
			i16 accum[5];
			MemSet(&accum);
#endif

			/* decode vertices */
			for (u32 dst_ofs_verts_end = dst_ofs_verts + src_part.num_verts; dst_ofs_verts < dst_ofs_verts_end; ++dst_ofs_verts) {
				/* separate XYZST streams */
				float unpack[5];
				for (u16 stream_index = 0; stream_index < 5; ++stream_index) {
					u16 raw_value = ReadVarint(src_pos[stream_index]);
#if DEMO_MODELS_USE_DELTA_ENCODING
					float div = (stream_index > 2) ? UVScale : PositionScale;
					accum[stream_index] += DecodeSignMagnitude(raw_value);
					unpack[stream_index] = float(accum[stream_index]) / div;
#else
					unpack[stream_index] = (stream_index < 3) ? DecodeSignMagnitude(raw_value) / 4.f : raw_value / 128.f;
#endif // DEMO_MODELS_USE_DELTA_ENCODING
				}

				/* flip upside down */
				unpack[4] = 1.f - unpack[4];

				MemCopy(dst_ofs_verts + Storage::vertices, unpack, 3 * sizeof(float));
				MemCopy(dst_ofs_verts + Storage::uvs, unpack + 3, 2 * sizeof(float));
			}

			/* decode indices */
			u16 watermark = 0;
			auto read_next_index = [&] {
				/* watermark delta decoding */
				u16 index = watermark + 3 - ReadVarint(src_idx);
				assign_max(watermark, index);
				return index;
			};

			for (u32 dst_ofs_idx_end = dst_ofs_idx + src_part.num_indices; dst_ofs_idx < dst_ofs_idx_end; /**/) {
				for (u32 i = 0; i < 3; ++i)
					Storage::indices[dst_ofs_idx++] = read_next_index();

				/* decode paired triangle */
				if (dst_ofs_idx + 2 < dst_ofs_idx_end && Storage::indices[dst_ofs_idx - 3] < Storage::indices[dst_ofs_idx - 2]) {
					Storage::indices[dst_ofs_idx + 0] = Storage::indices[dst_ofs_idx - 2];
					Storage::indices[dst_ofs_idx + 1] = Storage::indices[dst_ofs_idx - 3];
					Storage::indices[dst_ofs_idx + 2] = read_next_index();
					dst_ofs_idx += 3;
				}
			}

			/* compute normals */
			vec3* pos = Storage::vertices + dst_part.ofs_verts;
			vec3* nor = Storage::normals  + dst_part.ofs_verts;
			u32*  idx = Storage::indices  + dst_part.ofs_idx;

			if (!is_sprite) {
				ComputeNormals(pos, idx, dst_part.num_verts, dst_part.num_indices, nor);
			} else {
				vec3 center;
				MemSet(&center);
				for (u32 i = 0; i < dst_part.num_verts; ++i)
					center += pos[i];
				center /= float(dst_part.num_verts);
				for (u32 i = 0; i < dst_part.num_verts; ++i) {
					for (u8 j = 0; j < 3; ++j) {
						nor[i][j] = pos[i][j] - center[j];
						pos[i][j] = center[j];
					}
				}
			}
		}
	}

	assert(dst_ofs_verts == Storage::num_verts);
	assert(dst_ofs_idx == Storage::num_indices);
	assert(dst_ofs_part == Storage::num_parts);

	Storage::first_model_part[Model::Count] = dst_ofs_part;

	/* upload GPU data */
	Storage::gpu_addr.vertices = Gfx::UploadGeometry(Storage::vertices, Storage::num_verts,   Gfx::Arena::Permanent);
	Storage::gpu_addr.uvs      = Gfx::UploadGeometry(Storage::uvs,      Storage::num_verts,   Gfx::Arena::Permanent);
	Storage::gpu_addr.normals  = Gfx::UploadGeometry(Storage::normals,  Storage::num_verts,   Gfx::Arena::Permanent);
	Storage::gpu_addr.indices  = Gfx::UploadGeometry(Storage::indices,  Storage::num_indices, Gfx::Arena::Permanent);
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Model::Draw(Model::ID id, const Transform& transform) {
	// Note: using Math::ToRadians here would increase generated code size
	mat4 model_matrix = MakeRotation(transform.angles * Math::DEG2RAD);
	for (u16 i = 0; i < 3; ++i)
		mul(model_matrix.GetAxis(i), transform.scale);
	model_matrix.SetPosition(transform.position);

	Uniform::SetModelMatrix(model_matrix);

	const u16
		part_begin = Model::Storage::first_model_part[id],
		part_end   = Model::Storage::first_model_part[id + 1]
	;

	Gfx::Mesh mesh;
	memset(&mesh, 0, sizeof(mesh));

	for (u16 part_index = part_begin; part_index < part_end; ++part_index) {
		const Model::Part& part = Storage::parts[part_index];
		u16 material = part.material;

		u32 offset = part.ofs_verts;
		mesh.vertices[Attrib::Position	].SetData<vec3>(Storage::gpu_addr.vertices, offset);
		mesh.vertices[Attrib::TexCoord	].SetData<vec2>(Storage::gpu_addr.uvs,      offset);
		mesh.vertices[Attrib::Normal	].SetData<vec3>(Storage::gpu_addr.normals,  offset);

		mesh.index_addr			= Storage::gpu_addr.indices + part.ofs_idx * sizeof(u32);
		mesh.num_vertices		= part.num_verts;
		mesh.num_indices		= part.num_indices;

		Uniform::Texture0 = MaterialTextures[material];
		if (r_lightmap.value)
			Uniform::Texture0 = Texture::Grey;

		AddDrawCall(Material::ID(material), mesh);
	}
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Model::PackNormalOffset(vec3& normal, const vec3& offset) {
	for (u8 i = 0; i < 3; ++i) {
		normal[i] = floor(offset[i] * PositionScale) + normal[i] * 0.25f + 0.5f;
	}
}

FORCEINLINE void Demo::Model::UnpackNormal(const vec3& posnor, vec3& nor) {
	for (u8 i = 0; i < 3; ++i)
		nor[i] = (fract(posnor[i]) - 0.5f) * 4.f;
}
