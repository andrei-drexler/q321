#pragma once

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Map::TraceInfo::SetBullet(const vec3& a, const vec3& b) {
	start = a;
	delta = b - a;
	type = Type::Bullet;
}

FORCEINLINE void Demo::Map::TraceInfo::SetLightmap(const vec3& a, const vec3& b) {
	start = a;
	delta = b - a;
	type = Type::Lightmap;
}

FORCEINLINE void Demo::Map::TraceInfo::SetCollision(const vec3& a, const vec3& travel, const vec3& box) {
	MemCopy(&start, &a);
	MemCopy(&delta, &travel);
	MemCopy(&box_half_size, &box);
	type = Type::Collision;
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Map::Details::DoSplit(u16 node, const i16 bounds[2][3], u8 axis, i16 clip[2], i16& mid) {
	Partition::Node& parent = partition.nodes[node];
	assert(parent.IsLeaf());

	u16 left_cursor = parent.data[0];
	u16 right_cursor = parent.data[1];

	i32 clip_left = bounds[0][axis];
	i32 clip_right = bounds[1][axis];

	// avoid precision issues during traversal
	const i16 ClipBias = 1;

	i32 split_point_x2 = bounds[0][axis] + bounds[1][axis];
	while (left_cursor < right_cursor) {
		auto& brush_bounds = brushes.bounds[partition.brushes[left_cursor]];
		i32 brush_mid_x2 = brush_bounds[0][axis] + brush_bounds[1][axis];
		if (brush_mid_x2 > split_point_x2) {
			--right_cursor;
			assign_min(clip_right, brush_bounds[0][axis] - ClipBias);
			if (left_cursor < right_cursor)
				Swap(partition.brushes[left_cursor], partition.brushes[right_cursor]);
		} else {
			++left_cursor;
			assign_max(clip_left, brush_bounds[1][axis] + ClipBias);
		}
	}
	assert(left_cursor == right_cursor);

	clip[0] = clip_left;
	clip[1] = clip_right;
	mid = left_cursor;
}

NOINLINE void Demo::Map::Details::SplitNode(u16 index, i16 bounds[2][3]) {
	if (partition.num_nodes + 2 >= MAX_NUM_NODES)
		return;

	Partition::Node& parent = partition.nodes[index];
	assert(parent.IsLeaf());

	u16 left_cursor = parent.data[0];
	u16 right_cursor = parent.data[1];
	if (left_cursor + MIN_NODE_BRUSHES >= right_cursor)
		return;

	i16 mid;

	u8 best_axis = 0;
	i16 best_dist = 0;
	for (u8 axis = 0; axis < 4; ++axis) {
		DoSplit(index, bounds, axis == 3 ? best_axis : axis, parent.clip, mid);
		i16 dist = min(mid - left_cursor, right_cursor - mid);
		if (best_dist < dist) {
			best_dist = dist;
			best_axis = axis;
		}
	}

	if (best_dist == 0) {
		// We're not making any progress splitting on midpoint on any axis, so there's no reason to recurse.
		// Ideally, we'd check multiple splitting positions and weigh the results using SAH.
		return;
	}
	
	Partition::Node& left_child = partition.nodes[partition.num_nodes++];
	Partition::Node& right_child = partition.nodes[partition.num_nodes++];
	left_child.data[0] = parent.data[0];
	left_child.data[1] = mid;
	right_child.data[0] = mid;
	right_child.data[1] = parent.data[1];
	parent.data[0] = -(partition.num_nodes - 2);
	parent.data[1] = best_axis;

	auto tmp = bounds[1][best_axis];
	bounds[1][best_axis] = parent.clip[0];
	SplitNode(-parent.data[0], bounds);
	bounds[1][best_axis] = tmp;
	
	tmp = bounds[0][best_axis];
	bounds[0][best_axis] = parent.clip[1];
	SplitNode(-parent.data[0] + 1, bounds);
	bounds[0][best_axis] = tmp;
}

NOINLINE void Demo::Map::Details::CreatePartition() {
	for (u16 i = 0, count = brushes.count; i < count; ++i)
		partition.brushes[i] = i;

	auto& root = partition.nodes[0];
	root.data[0] = 0;
	root.data[1] = brushes.count;
	partition.num_nodes = 1;

	i16 bounds[2][3];
	MemCopy(&bounds, &brushes.world_bounds);

#ifndef DISABLE_PARTITION
	SplitNode(0, bounds);
#endif
}

FORCEINLINE bool Demo::Map::Details::TraceRayStep(TraceInfo& trace, u16 node_index, float tmin, float tmax) {
	bool hit = false;

	struct StackEntry {
		u16 node_index;
		float tmin;
		float tmax;
	};

	const u8 MaxDepth = 16;
	StackEntry stack[MaxDepth];
	u8 stack_top = 0;

beginning:
	auto& node = partition.nodes[node_index];
	if (tmin > tmax)
		goto pop;

	if (node.IsLeaf()) {
		for (auto i = node.data[0], endi = node.data[1]; i < endi; ++i) {
			auto brush_index = partition.brushes[i];
			i16 best_brush_plane = -1;

			// Note: when determining which entities we're touching
			// we don't want to ignore those we're already inside of
			// by a lot (t_enter < -1).
			// Same thing goes for lightmap traces.
			float t_enter = trace.max_touch_ents || trace.type == TraceType::Lightmap ? -FLT_MAX : -1.f;
			float t_exit = tmax;

			auto plane_index = brushes.start[brush_index];
			auto plane_index_end = brushes.start[brush_index + 1];

			for (; plane_index < plane_index_end; ++plane_index) {
				const vec4& plane = brushes.planes[plane_index];
				float dist =
					plane.w +
					plane.x * trace.start.x +
					plane.y * trace.start.y +
					plane.z * trace.start.z ;
				float align =
					plane.x * trace.delta.x +
					plane.y * trace.delta.y +
					plane.z * trace.delta.z ;

				if (trace.type == TraceType::Collision)
					for (u32 j = 0; j < 3; ++j)
						dist -= trace.box_half_size[j] * abs(plane[j]);

				if (align == 0.f) {
					if (dist > 0.f) {
						t_enter = FLT_MAX;
						break;
					}
					continue;
				}

				dist /= -align;
				if (align < 0.f) {
					if (t_enter < dist) {
						t_enter = dist;
						best_brush_plane = plane_index;
					}
				} else {
					if (t_exit > dist) {
						t_exit = dist;
					}
				}

				if (t_exit < t_enter)
					break;
			}
			
			if (best_brush_plane != -1 && t_exit > max(t_enter, 0.f) && t_enter < trace.fraction) {
				assert(best_brush_plane < (i32)brushes.plane_count);

				auto material = brushes.GetPlaneMaterial(best_brush_plane);
				auto props = Material::Properties[material];
				auto contents = props & Material::MaskContents;
				auto visibility = props & Material::MaskVisibility;

				if (trace.type == TraceType::Collision) {
					if (trace.num_touch_ents < trace.max_touch_ents) {
						u16 entity = brushes.entity[brush_index];
						u16 touch_index = 0;
						while (touch_index < trace.num_touch_ents) {
							if (trace.touch_ents[touch_index] == entity)
								break;
							else
								++touch_index;
						}
						if (touch_index == trace.num_touch_ents)
							trace.touch_ents[trace.num_touch_ents++] = entity;
					}

					if (trace.max_touch_ents && t_enter < -1.f) {
						// We're inside a solid, but the collision plane is too far behind us,
						// so we only record the touching entities without also reporting
						// a collision.
						// This prevents potential walljumping when pushing against a wall.

						continue;
					}

					if (contents < Material::PlayerClip)
						continue;
				} else if (trace.type == TraceType::Bullet) {
					if (contents != Material::WeaponClip)
						continue;
				} else if (trace.type == TraceType::Lightmap) {
					if (visibility >= Material::Translucent && (props & Material::BlocksLight) == 0)
						continue;
				}
				
				trace.plane = best_brush_plane;
				trace.fraction = t_enter;
				hit = true;
			}
		}

pop:
		if (stack_top == 0)
			return hit;

		auto& top = stack[--stack_top];
		node_index = top.node_index;
		tmin = top.tmin;
		tmax = min(top.tmax, trace.fraction);
		goto beginning;
	}

	u16 axis			= node.data[1];
	float ray_start		= trace.start[axis];
	float ray_delta		= trace.delta[axis];
	u8 flip				= (*(u32*)&trace.delta[axis]) >> 31;
	float enlarge		= trace.box_half_size[axis];

	float clip[2];
	clip[0]				= node.clip[0] + enlarge;
	clip[1]				= node.clip[1] - enlarge;

	float split_near	= (clip[flip] - ray_start) / ray_delta;
	float split_far		= (clip[!flip] - ray_start) / ray_delta;

	bool go_near		= split_near >= tmin;
	bool go_far			= split_far <= tmax;
	
	if (go_near) {
		if (go_far) {
			assert(stack_top < size(stack));
			auto& top = stack[stack_top++];
			top.node_index = node.GetChild(!flip);
			top.tmin = max(tmin, split_far);
			top.tmax = tmax;
		}
		node_index = node.GetChild(flip);
		assign_min(tmax, split_near);
		goto beginning;
	}
	if (go_far) {
		node_index = node.GetChild(!flip);
		assign_max(tmin, split_far);
		goto beginning;
	}

	goto pop;
}

NOINLINE bool Demo::Map::TraceRay(TraceInfo& trace) {
	trace.fraction = 1.f;
	trace.plane = -1;

	float travel = length(trace.delta);
	trace.start.z -= trace.z_offset;
	bool result = Details::TraceRayStep(trace, 0, 0.f, 1.f);
	trace.start.z += trace.z_offset;
	trace.start_solid = trace.fraction < 0.f;
	if (trace.start_solid)
		trace.fraction = 0.f;

	if (trace.fraction < 1.f) {
		trace.fraction -= 0.125f / travel;
		assign_max(trace.fraction, 0.f);
	}

	trace.hit_point.x = trace.start.x + trace.fraction * trace.delta.x;
	trace.hit_point.y = trace.start.y + trace.fraction * trace.delta.y;
	trace.hit_point.z = trace.start.z + trace.fraction * trace.delta.z;

	if (u16(trace.plane) < brushes.plane_count)
		trace.hit_normal = brushes.planes[trace.plane].xyz;

	// Add point entities to touch list.
	// Note: point entities are not partitioned,
	// so we simply iterate thorough all of them...

	for (u32 entity_index = Map::num_brush_entities; trace.num_touch_ents < trace.max_touch_ents && entity_index < Map::num_entities; ++entity_index) {
		const Demo::Entity& entity = Map::entities[entity_index];

		float dist = 0.f;
		for (u32 i = 0; i < 3; ++i)
			assign_max(dist, abs(trace.hit_point[i] - entity.origin[i]) - trace.box_half_size[i]);

		if (dist < Demo::Entity::Radius)
			trace.touch_ents[trace.num_touch_ents++] = entity_index;
	}

	return result;
}
