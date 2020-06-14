#pragma once

namespace Demo {
	struct PackedModel {
		struct Part {
			u8			shader;
			u16			num_verts;
			u16			num_indices;
		};

		u8				num_parts;
		u16				num_verts;
		u16				num_indices;

		const Part*		parts;
		const i16*		verts; // separate X/Y/Z/S/T streams
		const u16*		indices;
	};

	////////////////////////////////////////////////////////////////

	namespace Model {
		enum ID {
			#define PP_ADD_MODEL_ID(name,...) name,
			DEMO_MODELS(PP_ADD_MODEL_ID)
			#undef PP_ADD_MODEL_ID

			Count,
		};

		struct Part {
			u16					num_verts;
			u16					num_indices;
			u8					shader;
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
	};
}

////////////////////////////////////////////////////////////////
// Implementation //////////////////////////////////////////////
////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Model::LoadAll(const PackedModel* models) {
	assert(!Storage::parts);

	/* count resources */
	for (u16 model_index = 0; model_index < Model::Count; ++model_index) {
		const PackedModel& packed = models[model_index];
		Storage::num_verts += packed.num_verts;
		Storage::num_indices += packed.num_indices;
		Storage::num_parts += packed.num_parts;
	}

	/* alloc resources */
	Storage::vertices         = Mem::Alloc< vec3 >(Storage::num_verts);
	Storage::normals          = Mem::Alloc< vec3 >(Storage::num_verts);
	Storage::uvs              = Mem::Alloc< vec2 >(Storage::num_verts);
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

		vec3* pos = Storage::vertices + dst_ofs_verts;
		vec3* nor = Storage::normals + dst_ofs_verts;
		vec3* uv = Storage::normals + dst_ofs_verts;

		/* decode vertices */
		for (u16 vertex_index = 0; vertex_index < packed.num_verts; ++vertex_index) {
			const i16* src_pos = packed.verts + vertex_index;
			float unpack[5];

			// separate XYZST streams
			for (u16 stream_index = 0; stream_index < 5; ++stream_index, src_pos += packed.num_verts) {
				float div = (stream_index > 2) ? 128.f : 4.f;
				unpack[stream_index] = float(DecodeSignMagnitude(*src_pos)) / div;
			}

			MemCopy(pos + vertex_index, unpack, 3 * sizeof(float));
			MemCopy(uv + vertex_index, unpack + 3, 2 * sizeof(float));
		}

		u32* idx = Storage::indices + dst_ofs_idx;

		/* decode indices */
		for (u16 i = 0, index = 0; i < packed.num_indices; ++i) {
			index += DecodeSignMagnitude(packed.indices[i]);
			idx[i] = index;
		}
		
		for (u32 i = 0; i < packed.num_indices; i += 3, idx += 3) {
			constexpr u8 Next[] = {1, 2, 0, 1};
			for (u8 j = 0; j < 3; ++j) {
				u32 i0 = idx[j];
				u32 i1 = idx[Next[j]];
				u32 i2 = idx[Next[j + 1]];
				nor[i0] += cross(pos[i1] - pos[i0], pos[i2] - pos[i1]);
			}
		}

		for (u32 i = 0; i < packed.num_indices; ++i)
			safe_normalize(nor[i]);

		/* model -> part association */
		Storage::first_model_part[model_index] = dst_ofs_part;

		/* part properties */
		for (u16 part_index = 0; part_index < packed.num_parts; ++part_index) {
			const PackedModel::Part& src_part = packed.parts[part_index];
			Part& dst_part = Storage::parts[dst_ofs_part++];

			dst_part.num_verts = src_part.num_verts;
			dst_part.num_indices = src_part.num_indices;
			dst_part.ofs_verts = dst_ofs_verts;
			dst_part.ofs_idx = dst_ofs_idx;

			dst_ofs_verts += src_part.num_verts;
			dst_ofs_idx += src_part.num_indices;
		}
	}

	Storage::first_model_part[Model::Count] = dst_ofs_part;
}
