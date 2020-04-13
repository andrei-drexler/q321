#pragma once

NOINLINE void Map::LoadPatches(const PackedMap& packed, u8 pass) {
	u16 current_patch_vertex = 0;
	for (u16 patch_index = 0; patch_index < packed.num_patches; ++patch_index) {
		// HACK: we always mirror patches, instead of checking if we have to
		const bool Mirror = true;

		auto patch = packed.GetPatch(patch_index);
		u16 num_control_points = patch.width * patch.height;

		u8 prim_x = patch.width >> 1;
		u8 prim_y = patch.height >> 1;
		u32 verts_x = prim_x * patch.divx + 1;
		u32 verts_y = prim_y * patch.divy + 1;
		u32 num_verts = verts_x * verts_y * (1 + Mirror);

		if (pass == 0) {
			num_mat_verts[patch.material] += num_verts;
			num_mat_indices[patch.material] += (verts_x - 1) * (verts_y - 1) * (6 * (1 + Mirror));
			continue;
		}

		const u16 MaxPatchVertices = 64;
		PackedMap::PatchVertex ctrl[MaxPatchVertices];

		assert(num_control_points <= MaxPatchVertices);
		for (u16 i = 0; i < num_control_points; ++i)
			ctrl[i] = packed.GetPatchVertex(current_patch_vertex++);

		for (u8 mirror_side = 0; mirror_side < 1 + use_symmetry; ++mirror_side) {
			if (mirror_side == 1) {
				for (u16 i = 0; i < num_control_points; ++i)
					ctrl[i].pos[symmetry_axis] = 2 * symmetry_level - ctrl[i].pos[symmetry_axis];
			}

			auto first_vertex = num_mat_verts[patch.material];
			auto dst_index = patches.count++;
			assert(patches.count <= MAX_NUM_PATCHES);

			patches.source[dst_index] = (patch_index << 1) | mirror_side;
			patches.control_start[dst_index] = current_patch_vertex - num_control_points;
			patches.vertex_start[dst_index] = mat_vertex_offset[patch.material] + first_vertex;
			patches.vertex_count[dst_index] = verts_x * verts_y;
			
			for (u16 j = 2; j < patch.height; j += 2) {
				for (u8 sj = (j != 2); sj <= patch.divy; ++sj) {
					float t = sj / (float)patch.divy;
					float t_coeff[3] = {(1.f - t) * (1.f - t), 2.f * t * (1.f - t), t * t};

					PackedMap::PatchVertex* row = ctrl + patch.width * (j - 2);
					
					for (u16 i = 2; i < patch.width; i += 2, row += 2) {
						for (u8 si = (i != 2); si <= patch.divx; ++si) {
							float s = si / (float)patch.divx;
							float s_coeff[3] = {(1.f - s) * (1.f - s), 2.f * s * (1.f - s), s * s};

							auto index = mat_vertex_offset[patch.material] + num_mat_verts[patch.material]++;

							// Writing out these operations explicitly (instead of relying on vec3 operators)
							// reduces code size quite significantly (almost 200 bytes)...

							{
								vec3 v[3];
								auto c = row;
								for (u16 k = 0; k < 3; ++k, c += patch.width) {
									v[k].x = s_coeff[0]*c[0].pos.x + s_coeff[1]*c[1].pos.x + s_coeff[2]*c[2].pos.x;
									v[k].y = s_coeff[0]*c[0].pos.y + s_coeff[1]*c[1].pos.y + s_coeff[2]*c[2].pos.y;
									v[k].z = s_coeff[0]*c[0].pos.z + s_coeff[1]*c[1].pos.z + s_coeff[2]*c[2].pos.z;
								}
								positions[index].x = t_coeff[0]*v[0].x + t_coeff[1]*v[1].x + t_coeff[2]*v[2].x;
								positions[index].y = t_coeff[0]*v[0].y + t_coeff[1]*v[1].y + t_coeff[2]*v[2].y;
								positions[index].z = t_coeff[0]*v[0].z + t_coeff[1]*v[1].z + t_coeff[2]*v[2].z;
							}

							{
								vec2 v[3];
								auto c = row;
								for (u16 k = 0; k < 3; ++k, c += patch.width) {
									v[k].x = s_coeff[0]*c[0].uv.x + s_coeff[1]*c[1].uv.x + s_coeff[2]*c[2].uv.x;
									v[k].y = s_coeff[0]*c[0].uv.y + s_coeff[1]*c[1].uv.y + s_coeff[2]*c[2].uv.y;
								}
								texcoords[index].xy = t_coeff[0]*v[0] + t_coeff[1]*v[1] + t_coeff[2]*v[2];
								texcoords[index].z = (i - 2 + s * 2.f) / (patch.width - 1);
								texcoords[index].w = (j - 2 + t * 2.f) / (patch.height - 1);
							}
							int abcd = 0;
						}
					}
				}
			}

			u32 first_index = num_mat_indices[patch.material];
			auto* idx = indices + mat_index_offset[patch.material] + num_mat_indices[patch.material];
			u8 flip = mirror_side ? 3 : 0;

			for (u16 j = 1; j < verts_y; ++j) {
				for (u16 i = 1; i < verts_x; ++i) {
					idx[0		] = first_vertex + (j - 1) * verts_x + (i - 1);
					idx[1 ^ flip] = first_vertex + (j - 0) * verts_x + (i - 1);
					idx[2 ^ flip] = first_vertex + (j - 0) * verts_x + (i - 0);
					idx += 3;
					idx[0		] = first_vertex + (j - 1) * verts_x + (i - 1);
					idx[1 ^ flip] = first_vertex + (j - 0) * verts_x + (i - 0);
					idx[2 ^ flip] = first_vertex + (j - 1) * verts_x + (i - 0);
					idx += 3;
					num_mat_indices[patch.material] += 6;
				}
			}
		}
	}
}

