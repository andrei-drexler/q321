#pragma once

namespace Demo {
	NOINLINE void SetupCollisionTrace(Map::TraceInfo& trace) {
		MemSet(&trace);
		trace.type = Map::TraceType::Collision;
		trace.z_offset = Player::EyeCenterOffset;
		trace.box_half_size = Player::CollisionBounds;
	}

	NOINLINE void ClipVelocity(vec3& velocity, const vec3& normal) {
		const float Overclip = -513.f/512.f;
		float into = dot(velocity, normal);
		if (into < 0.f)
			mad(velocity, normal, into * Overclip);
	}

	bool GroundTrace(Player& player) {
		Map::TraceInfo trace;
		SetupCollisionTrace(trace);
		trace.start = player.position;
		trace.delta.z = -0.5f;
		trace.max_touch_ents = Player::MaxTouchEnts;
		trace.touch_ents = player.touch_ents;

		bool hit = Map::TraceRay(trace);
		if (hit) {
			player.ground = Map::brushes.planes + trace.plane;
		} else {
			player.ground = nullptr;
		}
		player.num_touch_ents = trace.num_touch_ents;

		return hit;
	}

	////////////////////////////////////////////////////////////////

	enum class MoveResult {
		Ok,
		Blocked,
	};

	MoveResult SlideMove(Player& player, float dt) {
		MoveResult result = MoveResult::Ok;

		if (!player.ground || player.ground->z < 0.5f)
			player.velocity.z -= g_gravity.value * dt;
		else
			ClipVelocity(player.velocity, player.ground->xyz);

		for (int bump = 0; bump < 3; ++bump) {
			Map::TraceInfo trace;
			SetupCollisionTrace(trace);

			trace.start = player.position;
			trace.delta = player.velocity * dt;
			Map::TraceRay(trace);

			if (trace.plane != -1)
				ClipVelocity(player.velocity, trace.hit_normal);

			player.position = trace.hit_point;

			if (trace.fraction == 1.f)
				break;

			dt *= 1.f - trace.fraction;
			result = MoveResult::Blocked;
		}

		return result;
	}

	////////////////////////////////////////////////////////////////

	void StepSlideMove(Player& player, float dt) {
		if (player.ground && player.velocity.z <= 0.f) {
			ClipVelocity(player.velocity, player.ground->xyz);
		}

		vec3 pos = player.position;
		vec3 vel = player.velocity;

		if (SlideMove(player, dt) == MoveResult::Ok)
			return;

		const float StepSize = 18.f;

		Map::TraceInfo trace;

		MemSet(&trace);
		trace.start = pos;
		trace.z_offset = Player::EyeCenterOffset;
		trace.delta.z = -StepSize;
		trace.box_half_size = Player::CollisionBounds;
		trace.type = Map::TraceInfo::Type::Collision;
		Map::TraceRay(trace);

		// never step up when you still have up velocity
		if (player.velocity.z > 0.f && (trace.fraction == 1.f || trace.hit_normal.z < 0.71875f)) // 0.7
			return;

		// test the player position if they were a stepheight higher
		trace.delta.z = StepSize;
		Map::TraceRay(trace);
		if (trace.fraction < 1.f)
			return;

		player.position = trace.hit_point;
		player.velocity = vel;
		SlideMove(player, dt);

		// push down the final amount
		trace.start = player.position;
		trace.delta.z = -StepSize;
		Map::TraceRay(trace);
		if (trace.fraction > 0.f) {
			player.position = trace.hit_point;
		}

		if (trace.fraction < 1.f)
			ClipVelocity(player.velocity, trace.hit_normal);

		float delta = player.position.z - pos.z;
		if (delta > 0.5f)
			player.step += delta;
	}
} // namespace Demo