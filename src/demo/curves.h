#pragma once

////////////////////////////////////////////////////////////////

namespace DeCasteljau {
	static constexpr u8 MixSteps[] = {
		#define PP_MIX(dst, first, second, axis)  dst, first, ((second - first) << 1) | axis

		/*
		3x3 -> 2x3 (horizontal mix)

		O->-x--O->-x--O
		|   .  |   .  |
		|   .  |   .  |
		O->-x--O->-x--O
		|   .  |   .  |
		|   .  |   .  |
		O->-x--O->-x--O
		*/
		PP_MIX(0, 0, 1, 0),
		PP_MIX(1, 1, 2, 0),
		PP_MIX(2, 3, 4, 0),
		PP_MIX(3, 4, 5, 0),
		PP_MIX(4, 6, 7, 0),
		PP_MIX(5, 7, 8, 0),

		/*
		2x3 -> 2x2 (vertical mix)

		O-------O
		|       |
		v       v
		x . . . x
		|       |
		O-------O
		|       |
		v       v
		x . . . x
		|       |
		O-------O
		*/
		PP_MIX(0, 0, 2, 1),
		PP_MIX(1, 1, 3, 1),
		PP_MIX(2, 2, 4, 1),
		PP_MIX(3, 3, 5, 1),

		/*
		edge points (horizontal & vertical)

		O-->--x---O
		|     .   |
		v     .   v
		x . . . . x
		|     .   |
		O-->--x---O
		*/
		PP_MIX(4, 0, 1, 0), // top edge
		PP_MIX(5, 2, 3, 0), // bottom edge
		PP_MIX(6, 0, 2, 1), // left edge
		PP_MIX(7, 1, 3, 1), // right edge

		PP_MIX(8, 4, 5, 1), // final position

		#undef PP_MIX
	};
}

////////////////////////////////////////////////////////////////

NOINLINE void Demo::Map::Details::EvaluatePatch(const PackedMap::Patch& patch, const PackedMap::PatchVertex* ctrl, float s, float t, vec3& pos, vec3& nor, vec2& uv) {
	/* determine cell index */
	{
		u32 prim_x = patch.width >> 1;
		u32 prim_y = patch.height >> 1;

		s *= prim_x;
		t *= prim_y;
		float ix = floor(s);
		float iy = floor(t);
		i32 x = (i32)ix;
		i32 y = (i32)iy;
		s -= ix;
		t -= iy;

		assert(u32(x) <= prim_x);
		assert(u32(y) <= prim_y);

		if (x == prim_x) {
			--x;
			++s;
		}
		if (y == prim_y) {
			--y;
			++t;
		}

		ctrl += (patch.width * y + x) * 2;
	}

	/* copy initial control points */
	PackedMap::PatchVertex c[9];
	for (u32 y = 0; y < 3; ++y, ctrl += patch.width)
		MemCopy(c + y * 3, ctrl, 3);

	/* apply control point mixing steps */
	float coeff[2] = {s, t};
	for (u32 k = 0; k < size(DeCasteljau::MixSteps); k += 3) {
		u32 dst    = DeCasteljau::MixSteps[k    ];
		u32 first  = DeCasteljau::MixSteps[k + 1];
		u32 delta  = DeCasteljau::MixSteps[k + 2];
		u32 axis   = delta & 1;
		u32 second = (delta >> 1) + first;

		float f = coeff[axis];
		u32 field = 0;
		do {
			c[dst].data[field] = mix(c[first].data[field], c[second].data[field], f);
		} while (++field < size(c[dst].data));
	}

	safe_normalize(cross(
		c[5].pos - c[4].pos, // dt
		c[7].pos - c[6].pos  // ds
	), nor);

	pos = c[8].pos;
	uv  = c[8].uv;
}

////////////////////////////////////////////////////////////////

NOINLINE void Demo::Map::Details::LoadPatches(const PackedMap& packed, u8 pass) {
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

					EvaluatePatch(patch, ctrl, s, t, pos, nor, uv.xy);
					uv.z = s;
					uv.w = t;
				}
			}

			/* build index buffer */
			auto* idx = indices + mat_index_offset[patch.material] + num_mat_indices[patch.material];

			for (u16 j = 1; j < verts_y; ++j) {
				for (u16 i = 1; i < verts_x; ++i) {
					u32 quad_mask = 0b00'10'11'01'00'11'10'00'11'00'01'11;
					if (mirror_side)
						quad_mask >>= 12;

					for (u16 k = 0; k < 6; ++k) {
						u16 x = quad_mask & 1; quad_mask >>= 1;
						u16 y = quad_mask & 1; quad_mask >>= 1;
						*idx++ = first_vertex + (j - y) * verts_x + (i - x);
					}

					num_mat_indices[patch.material] += 6;
				}
			}
		}
	}
}

