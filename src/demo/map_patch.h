#pragma once

////////////////////////////////////////////////////////////////

NOINLINE void Map::Details::EvaluatePatch(const PackedMap::Patch& patch, const PackedMap::PatchVertex* ctrl, float s, float t, vec3& pos, vec3& nor, vec2& uv) {
	u8 prim_x = patch.width >> 1;
	u8 prim_y = patch.height >> 1;

	s *= prim_x;
	t *= prim_y;
	float is = floor(s);
	float it = floor(t);
	i16 i = (i16)is;
	i16 j = (i16)it;
	s -= is;
	t -= it;

	assert(i <= prim_x);
	assert(j <= prim_y);

	if (i == prim_x) {
		--i;
		++s;
	}
	if (j == prim_y) {
		--j;
		++t;
	}

	i *= 2;
	j *= 2;

	const PackedMap::PatchVertex* row = ctrl + patch.width * j + i;

	float t_coeff[3] = {(1.f - t) * (1.f - t), 2.f * t * (1.f - t), t * t};
	float s_coeff[3] = {(1.f - s) * (1.f - s), 2.f * s * (1.f - s), s * s};

	// Writing out these operations explicitly (instead of relying on vec3 operators)
	// reduces code size quite significantly (almost 200 bytes)...

	// TODO: DeCasteljau

	{
		vec3 v[3];
		auto c = row;
		for (u16 k = 0; k < 3; ++k, c += patch.width) {
			v[k].x = s_coeff[0]*c[0].pos.x + s_coeff[1]*c[1].pos.x + s_coeff[2]*c[2].pos.x;
			v[k].y = s_coeff[0]*c[0].pos.y + s_coeff[1]*c[1].pos.y + s_coeff[2]*c[2].pos.y;
			v[k].z = s_coeff[0]*c[0].pos.z + s_coeff[1]*c[1].pos.z + s_coeff[2]*c[2].pos.z;
		}
		pos.x = t_coeff[0]*v[0].x + t_coeff[1]*v[1].x + t_coeff[2]*v[2].x;
		pos.y = t_coeff[0]*v[0].y + t_coeff[1]*v[1].y + t_coeff[2]*v[2].y;
		pos.z = t_coeff[0]*v[0].z + t_coeff[1]*v[1].z + t_coeff[2]*v[2].z;

		vec3 dt = t_coeff[0] * (v[1] - v[0]) + t_coeff[2] * (v[2] - v[1]);

		c = row;
		for (u16 k = 0; k < 3; ++k, ++c) {
			v[k].x = t_coeff[0]*c[0].pos.x + t_coeff[1]*c[patch.width].pos.x + t_coeff[2]*c[patch.width*2].pos.x;
			v[k].y = t_coeff[0]*c[0].pos.y + t_coeff[1]*c[patch.width].pos.y + t_coeff[2]*c[patch.width*2].pos.y;
			v[k].z = t_coeff[0]*c[0].pos.z + t_coeff[1]*c[patch.width].pos.z + t_coeff[2]*c[patch.width*2].pos.z;
		}
		vec3 ds = s_coeff[0] * (v[1] - v[0]) + s_coeff[2] * (v[2] - v[1]);
		safe_normalize(cross(dt, ds), nor);
	}

	{
		vec2 v[3];
		auto c = row;
		for (u16 k = 0; k < 3; ++k, c += patch.width) {
			v[k].x = s_coeff[0]*c[0].uv.x + s_coeff[1]*c[1].uv.x + s_coeff[2]*c[2].uv.x;
			v[k].y = s_coeff[0]*c[0].uv.y + s_coeff[1]*c[1].uv.y + s_coeff[2]*c[2].uv.y;
		}
		uv = t_coeff[0]*v[0] + t_coeff[1]*v[1] + t_coeff[2]*v[2];
	}
}

////////////////////////////////////////////////////////////////

NOINLINE void Map::Details::LoadPatches(const PackedMap& packed, u8 pass) {
	patches.count = 0;

	for (u16 patch_index = 0, current_patch_vertex = 0; patch_index < packed.num_patches; ++patch_index) {
		PackedMap::Patch patch;
		packed.GetPatch(patch, patch_index);
		u16 num_control_points = patch.width * patch.height;
		bool mirror = UseSymmetry() && !patch.asymmetric;

		const u16 MaxPatchVertices = 64;
		PackedMap::PatchVertex ctrl[MaxPatchVertices];

		assert(num_control_points <= MaxPatchVertices);
		for (u16 i = 0; i < num_control_points; ++i) {
			packed.GetPatchVertex(ctrl[i], patch, current_patch_vertex++);
			if (mirror)
				mirror = ctrl[i].pos[symmetry_axis] < symmetry_level + 1;
		}

		u8 prim_x = patch.width >> 1;
		u8 prim_y = patch.height >> 1;
		u32 verts_x = prim_x * patch.divx + 1;
		u32 verts_y = prim_y * patch.divy + 1;
		u32 num_verts = verts_x * verts_y * (1 + mirror);

		if (pass == 0) {
			num_mat_verts[patch.material] += num_verts;
			num_mat_indices[patch.material] += (verts_x - 1) * (verts_y - 1) * (6 * (1 + mirror));
			continue;
		}

		for (u8 mirror_side = 0; mirror_side < 1 + mirror; ++mirror_side) {
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

			/* evaluate vertices */
			for (u16 j = 0; j < verts_y; ++j) {
				float t = j / float(verts_y - 1);

				for (u16 i = 0; i < verts_x; ++i) {
					float s = i / float(verts_x - 1);
					
					auto index = mat_vertex_offset[patch.material] + num_mat_verts[patch.material]++;
					vec3& pos = positions[index];
					vec4& uv = texcoords[index];
					vec3 nor;

					EvaluatePatch(patch, ctrl, s, t, positions[index], nor, uv.xy);

					uv.z = s;
					uv.w = t;
				}
			}

			/* build index buffer */
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

