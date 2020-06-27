#pragma once

////////////////////////////////////////////////////////////////

NOINLINE void MixControlPoint(PackedMap::PatchVertex& a, const PackedMap::PatchVertex& b, float f) {
	mix_into(a.pos, b.pos, f);
	mix_into(a.uv, b.uv, f);
}

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

	const PackedMap::PatchVertex* src = ctrl + patch.width * j + i;
	PackedMap::PatchVertex c[6];

	for (u8 y = 0; y < 3; ++y, src += patch.width) {
		PackedMap::PatchVertex* dst = c + y * 2;
		MemCopy(dst, src, 2);
		MixControlPoint(dst[0], src[1], s);
		MixControlPoint(dst[1], src[2], s);
	}

	for (u8 y = 0; y < 4; ++y)
		MixControlPoint(c[y], c[y + 2], t);

	vec3 ds = c[1].pos - c[0].pos;
	mix_into(ds, c[3].pos - c[2].pos, t);
	
	vec3 dt = c[2].pos - c[0].pos;
	mix_into(dt, c[3].pos - c[1].pos, s);
	safe_normalize(cross(dt, ds), nor);

	for (u8 y = 0; y < 2; ++y)
		MixControlPoint(c[y * 2], c[y * 2 + 1], s);
	MixControlPoint(c[0], c[2], t);

	pos = c[0].pos;
	uv = c[0].uv;
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

