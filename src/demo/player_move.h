#pragma once

namespace Demo {
	void ClipVelocity(vec3& velocity, const vec3& normal) {
		const float Overclip = 513.f/512.f;
		float into = dot(velocity, normal);
		if (into < 0.f)
			into *= Overclip;
		else
			into /= Overclip;
		velocity.x -= normal.x * into;
		velocity.y -= normal.y * into;
		velocity.z -= normal.z * into;
	}

	////////////////////////////////////////////////////////////////
	
	bool SlideMove(Player& player, float dt) {
		bool noclip = false;
		if (noclip) {
			player.position += player.velocity * dt;
			return false;
		}

		const u8 MaxClipPlanes = 5;
		vec3 planes[MaxClipPlanes];
		u8 num_planes = 0, bump, i;

		safe_normalize(player.velocity, planes[num_planes++]);

		const float
			ThreshIgnore	= 1./16.f,		// ~ 0.1
			ThreshSame		= 0.984375f;	// ~ 0.99

		for (bump = 0; bump < 4; ++bump) {
			vec3 advance = player.velocity * dt;

			Map::TraceInfo trace;
			player.position.z -= Player::EyeCenterOffset;
			trace.SetCollision(player.position, advance, Player::CollisionBounds);
			player.position.z += Player::EyeCenterOffset;

			bool hit = g_map.TraceRay(trace);
			trace.hit_point.z += Player::EyeCenterOffset;

			if (trace.fraction > 0.f) {
				// actually covered some distance
				player.position = trace.hit_point;
			}
			
			if (trace.start_solid) {
				//return true;
			}

			if (trace.fraction == 1.f) {
				// moved the entire distance
				break;
			}

			dt *= 1.f - trace.fraction;

			if (num_planes >= MaxClipPlanes) {
				// this shouldn't really happen
				player.velocity = 0.f;
				return true;
			}

			// if this is the same plane we hit before, nudge velocity
			// out along it, which fixes some epsilon issues with
			// non-axial planes
			for (i = 0; i<num_planes; ++i) {
				if (dot(trace.hit_normal, planes[i]) > ThreshSame) {
					player.velocity += trace.hit_normal;
					break;
				}
			}

			if (i < num_planes)
				continue;

			planes[num_planes++] = trace.hit_normal;

			//
			// modify velocity so it parallels all of the clip planes
			//

			// find a plane that it enters
			for (i = 0; i < num_planes; ++i) {
				if (dot(player.velocity, planes[i]) >= ThreshIgnore) {
					// move doesn't interact with the plane
					continue;
				}

				vec3 clip_velocity = player.velocity;

				// slide along the plane
				ClipVelocity(clip_velocity, planes[i]);

				// see if there is a second plane that the new move enters
				for (u8 j = 0; j < num_planes; ++j) {
					if (j == i)
						continue;

					if (dot(clip_velocity, planes[j]) >= ThreshIgnore) {
						// move doesn't interact with the plane
						continue;
					}

					// try clipping the move to the plane
					ClipVelocity(clip_velocity, planes[j]);

					// see if it goes back into the first clip plane
					if (dot(clip_velocity, planes[i]) >= 0.f)
						continue;

					// slide the original velocity along the crease
					safe_normalize(cross(planes[i], planes[j]), clip_velocity);
					clip_velocity *= dot(clip_velocity, player.velocity);

					// see if there is a third plane that the new move enters
					for (u8 k = 0; k < num_planes; ++k) {
						if (k == i || k == j)
							continue;
					
						if (dot(clip_velocity, planes[k]) >= ThreshIgnore) {
							// move doesn't interact with the plane
							continue;
						}

						// stop dead at a triple plane interaction
						player.velocity = 0.f;
						return true;
					}
				}

				// if we have fixed all interactions, try another move
				player.velocity = clip_velocity;
				break;
			}
		}

		return bump != 0;
	}

	////////////////////////////////////////////////////////////////

	void StepSlideMove(Player& player, float dt) {
		vec3 pos = player.position;
		vec3 vel = player.velocity;

		if (!SlideMove(player, dt))
			return;
		
		const float StepSize = 18.f;
		const vec3 up = {0.f, 0.f, StepSize};
		const vec3 down = {0.f, 0.f, -StepSize};

		Map::TraceInfo trace;
#if 0
		trace.SetCollision(pos, down, Player::CollisionBounds);
		g_map.TraceRay(trace);
		
		// never step up when you still have up velocity
		if (player.velocity.z > 0.f && (trace.fraction == 1.f || trace.hit_normal.z < 0.7f))
			return;
#endif

		trace.SetCollision(pos, up, Player::CollisionBounds);
		g_map.TraceRay(trace);
		if (trace.fraction < 1.f)
			return;

		player.position = trace.hit_point;
		player.velocity = vel;
		SlideMove(player, dt);

		trace.SetCollision(player.position, down, Player::CollisionBounds);
		g_map.TraceRay(trace);
		if (trace.fraction > 0.f) {
			player.position = trace.hit_point;
			player.position.z += 3.f;
		}

		if (trace.fraction < 1.f)
			ClipVelocity(player.velocity, trace.hit_normal);

		float delta = player.position.z - pos.z;
		if (abs(delta) > 0.5f) {
			int abcd = 0;
		}
	}
} // namespace Demo