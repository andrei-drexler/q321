#pragma once

struct PackedMap {
	enum {
		EnableSpotlights = true,
	};

	u16					num_brushes;
	u16					num_unaligned_planes;
	u16					num_planes;
	u16					num_patches;
	u16					num_patch_verts;
	u8					num_entities;
	u8					num_brush_entities;
	u8					num_uvs;
	u8					num_lights;
	u8					num_spotlights;
	u32					skylight;

	const u16*			entity_brushes;
	const i16*			entity_data;
	const i16*			world_bounds;
	const i16*			brush_bounds;
	const i32*			plane_data;
	const u8*			nonaxial_counts;
	const u16*			plane_materials;
	const float*		uv_data;
	const u8*			plane_uvs;
	const u32*			patches;
	const i16*			patch_vertices;
	const i16*			light_data;

	const u8*			brush_asymmetry;
	i8					symmetry_axis;
	i16					symmetry_level;

#ifdef DEV
	u16					num_plane_entries;
#endif

	struct {
		i16				position[3];
		i16				angles[2];
	}					levelshot;
	const char*			name;
	const char*			message;

	template
	<
		int NumBrushEntities, int NumEntityDataEntries,
		int NumBrushBoundEntries, int NumPlaneEntries, int NumNonaxialEntries,
		int NumUVEntries, int NumPlaneUVEntries, int NumMaterialEntries,
		int NumPatches, int NumPatchVertEntries,
		int NumLightEntries,
		int NumAsymmetryEntries
	>
	constexpr PackedMap
	(
		const char*						name,
		const char*						message,
		i8								symmetry_axis,
		i16								symmetry_level,
		const u16	(&entity_brushes)	[NumBrushEntities],
		const i16	(&entity_data)		[NumEntityDataEntries],
		const i16	(&world_bounds)		[6],
		const i16	(&brush_bounds)		[NumBrushBoundEntries],
		u16								num_unaligned_planes,
		const i32	(&plane_data)		[NumPlaneEntries],
		const u8	(&nonaxial_counts)	[NumNonaxialEntries],
		const u8	(&brush_asymmetry)	[NumAsymmetryEntries],
		const u16	(&plane_materials)	[NumMaterialEntries],
		const float (&uv_data)			[NumUVEntries],
		const u8	(&plane_uvs)		[NumPlaneUVEntries],
		const u32	(&patches)			[NumPatches],
		const i16	(&patch_verts)		[NumPatchVertEntries],
		const i16	(&light_data)		[NumLightEntries],
		u8								num_spotlights,
		u32								skylight,
		i16								levelshot_x,
		i16								levelshot_y,
		i16								levelshot_z,
		i16								levelshot_yaw,
		i16								levelshot_pitch
	) :
		name					(name),
		message					(message),
		entity_brushes			(entity_brushes),
		num_brush_entities		(NumBrushEntities),
		entity_data				(entity_data),
		num_entities			(NumEntityDataEntries / (sizeof(Demo::Entity) / sizeof(i16))),
		world_bounds			(world_bounds),
		brush_bounds			(brush_bounds),
		num_brushes				(NumBrushBoundEntries / 6),
		plane_data				(plane_data),
		num_unaligned_planes	(num_unaligned_planes),
#ifdef DEV
		num_plane_entries		(NumPlaneEntries),
#endif
		nonaxial_counts			(nonaxial_counts),
		uv_data					(uv_data),
		num_uvs					(NumUVEntries / 5),
		plane_uvs				(plane_uvs),
		plane_materials			(plane_materials),
		num_planes				(NumMaterialEntries),
		num_patches				(NumPatches),
		patches					(patches),
		patch_vertices			(patch_verts),
		num_patch_verts			(NumPatchVertEntries / 5),
		light_data				(light_data),
		num_lights				((NumLightEntries - num_spotlights * 3) / 5),
		num_spotlights			(num_spotlights),
		skylight				(skylight),
		symmetry_axis			(symmetry_axis),
		symmetry_level			(symmetry_level),
		brush_asymmetry			(brush_asymmetry)
	{
		static_assert(NumPlaneUVEntries == NumMaterialEntries);
		static_assert(NumBrushBoundEntries / 6 == NumNonaxialEntries);
		
		levelshot.position[0] = levelshot_x;
		levelshot.position[1] = levelshot_y;
		levelshot.position[2] = levelshot_z;
		levelshot.angles[0] = levelshot_yaw;
		levelshot.angles[1] = levelshot_pitch;
	}

	struct UV {
		vec2	offset;
		vec2	scale;
		float	angle;
	};

	struct Patch {
		u8		width;
		u8		height;
		u8		divx;
		u8		divy;
		u8		material;
		bool	asymmetric;

		/* used for delta decoding */
		struct {
			i16	pos[3];
			i16	uv[2];
		} delta;
	};

	struct PatchVertex {
		vec3	pos;
		vec2	uv;
	};

	struct Light {
		enum Flags {
			IsSpotlight = 1 << 0,
		};

		vec3	position;
		u32		flags;
		vec3	color;
		float	intensity;
		vec4	spot;
	};

	UV							GetPlaneUV(u32 plane_index) const;
	Patch						GetPatch(u32 patch_index) const;
	PatchVertex					GetPatchVertex(Patch& patch, u32 vertex_index) const;
	void						GetLight(u32 light_index, Light& light) const;
	void						GetPlane(const i32*& plane_data, const i16 brush_bounds[2][3], vec4& plane) const;
};

////////////////////////////////////////////////////////////////

FORCEINLINE PackedMap::UV PackedMap::GetPlaneUV(u32 plane_index) const {
	const float* data = uv_data + plane_uvs[plane_index];

	UV uv;
	uv.offset.x	= *data; data += num_uvs;
	uv.offset.y	= *data; data += num_uvs;
	uv.angle	= *data; data += num_uvs;
	uv.scale.x	= *data; data += num_uvs;
	uv.scale.y	= *data;

	uv.angle	*= -Math::DEG2RAD;

	return uv;
}

NOINLINE PackedMap::Patch PackedMap::GetPatch(u32 patch_index) const {
	u32 data = patches[patch_index];

	Patch patch;
	MemSet(&patch);

	patch.width			= ((data & 7) << 1) + 3;
	patch.height		= (((data >> 3) & 7) << 1) + 3;
	patch.divx			= 1 << ((data >> 6) & 7);
	patch.divy			= 1 << ((data >> 9) & 7);
	patch.material		= data >> 12;
	patch.asymmetric	= (data >> 20) & 1;

	return patch;
}

NOINLINE PackedMap::PatchVertex PackedMap::GetPatchVertex(Patch& patch, u32 vertex_index) const {
	const i16* data = patch_vertices + vertex_index;

	PatchVertex v;
	patch.delta.pos[0] += DecodeSignMagnitude(*data); data += num_patch_verts;
	patch.delta.pos[1] += DecodeSignMagnitude(*data); data += num_patch_verts;
	patch.delta.pos[2] += DecodeSignMagnitude(*data); data += num_patch_verts;
	patch.delta.uv[0]  += DecodeSignMagnitude(*data); data += num_patch_verts;
	patch.delta.uv[1]  += DecodeSignMagnitude(*data);

	// delta decoding
	v.pos[0] = patch.delta.pos[0];
	v.pos[1] = patch.delta.pos[1];
	v.pos[2] = patch.delta.pos[2];
	v.uv[0] =  patch.delta.uv[0] / 256.f;
	v.uv[1] = -patch.delta.uv[1] / 256.f; // flip upside-down

	return v;
}

FORCEINLINE void PackedMap::GetLight(u32 light_index, Light& light) const {
	const i16* data = light_data + light_index * 5;

	light.position.x	= data[0];
	light.position.y	= data[1];
	light.position.z	= data[2];
	light.intensity		= data[3];
	light.color.x		= ((data[4] >>  0) & 31) / 31.f;
	light.color.y		= ((data[4] >>  5) & 31) / 31.f;
	light.color.z		= ((data[4] >> 10) & 31) / 31.f;

	u8 first_spotlight = num_lights - num_spotlights;
	light_index -= first_spotlight;
	if (EnableSpotlights) {
		if (light_index < num_spotlights) {
			data = light_data + num_lights * 5 + light_index * 3;
		
			light.flags		= Light::IsSpotlight;
			light.spot.x	= data[0];
			light.spot.y	= data[1];
			light.spot.z	= data[2];
			light.spot.w	= length(light.spot.xyz);
			light.spot.xyz /= light.spot.w;
		} else {
			light.flags		= 0;
			light.spot		= 0.f;
		}
	} else {
		light.flags = (light_index < num_spotlights) ? Light::IsSpotlight : 0;
	}
}

FORCEINLINE void PackedMap::GetPlane(const i32*& plane_data, const i16 brush_bounds[2][3], vec4& plane) const {
	u32 value = *plane_data++;
	u8 axis0 = value & 3;
	if (axis0 != 3) {
		const u32
			OffsetBits		= 12,
			OffsetMask		= (1 << OffsetBits) - 1
		;

		bool negative_axis1 = (value >> 2) & 1;
		bool negative_axis2 = (value >> 3) & 1;
		float offset1 = (value >> 4) & OffsetMask;
		float offset2 = value >> 16;

		const u8 NextAxis[] = {1, 2, 0, 1};
		u8 axis1 = NextAxis[axis0];
		u8 axis2 = NextAxis[axis1];

		vec3 corner;
		corner[axis0] = brush_bounds[0][axis0];
		corner[axis1] = brush_bounds[!negative_axis1][axis1];
		corner[axis2] = brush_bounds[!negative_axis2][axis2];

		corner[axis1] += negative_axis1 ? offset1 : -offset1;

		float norm = sqrt(offset1*offset1 + offset2*offset2);
		offset1 /= norm;
		offset2 /= norm;

		plane[axis0] = 0.f;
		// rotate vec2(offset1, offset2) by 90 degrees and adjust sign
		plane[axis1] = negative_axis1 ? -offset2 : offset2; // not a typo!
		plane[axis2] = negative_axis2 ? -offset1 : offset1; // not a typo!

		plane.w = -dot(plane.xyz, corner);
	} else {
		value >>= 2;
		i32 w = *plane_data++;

		const i32
			OctBits			= 12,
			OctMask			= (1 << OctBits) - 1,
			OctMaxValue		= OctMask >> 1,
			DistFractBits	= 4,
			DistScale		= 1 << DistFractBits;

		vec2 oct;
		i32 x = value & OctMask;
		i32 y = value >> 16;
		oct.x = DecodeSignMagnitude(x) / float(OctMaxValue);
		oct.y = DecodeSignMagnitude(y) / float(OctMaxValue);
		assert(oct.x >= -1.f && oct.x <= 1.f);
		assert(oct.y >= -1.f && oct.y <= 1.f);

		plane.xyz = oct_to_vec3(oct);
		plane.w = DecodeSignMagnitude(w) * (1.f / float(DistScale));
	}
}

