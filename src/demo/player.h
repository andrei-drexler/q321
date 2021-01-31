#pragma once

namespace Demo {
	struct Player {
		static constexpr float
			MoveSpeed		= 320.f,
			StepUpSpeed		= 32.f,
			TurnSpeed		= 90.f,
			TeleportSpeed	= 400.f,
			JumpSpeed		= 270.f,
			LandSpeed		= 200.f,
			StopSpeed		= 100.f,

			GroundAccel		= 10.f,
			AirAccel		= 1.f,
			Friction		= 6.f,

			NoClipAccel		= 8.f,

			LandDeflectTime	= 0.15625f,		// 0.15 in Q3
			LandReturnTime	= 0.3125f,		// 0.3
			LandTime		= LandDeflectTime + LandReturnTime,

			TeleportTime	= 1.f / 4.f,

			Height			= 56.f,
			HalfHeight		= Height * 0.5f,
			HalfWidth		= 15.f,
			EyeLevel		= 48.f,
			EyeCenterOffset	= EyeLevel - HalfHeight,
			SpawnOffset		= HalfHeight - 4.f,

			ZoomTime		= 0.125f
		;

		static constexpr vec3
			CollisionBounds = {HalfWidth, HalfWidth, HalfHeight};

		enum Flag {
			EnableNoClip	= true, // disable to save ~86 bytes
			
			NoJump			= 1 << 0,
			NoClip			= EnableNoClip << 1,
		};

		bool NoClipping() const {
			if constexpr (EnableNoClip)
				return (flags & Flag::NoClip) != 0;
			else
				return false;
		}

		enum class Input : u8 {
			MoveForward,	// +X
			MoveBack,		// -X
			MoveRight,		// +Y
			MoveLeft,		// -Y
			MoveUp,			// +Z
			MoveDown,		// -Z
			LookLeft,		// +Yaw
			LookRight,		// -Yaw
			LookUp,			// +Pitch
			LookDown,		// -Pitch
			LookCenter,
			Run,
			Zoom,

			Count,
			Invalid = Count,
		};

		static constexpr bool IsValid(Input i) {
			return u32(i) < u32(Input::Invalid);
		}

		vec3				position;
		vec3				velocity;
		float				step;
		float				walk_cycle;
		float				land_change;
		float				land_time;
		float				shadow_angle;
		float				zoom;
		float				teleport;
		const vec4*			ground;
		vec3				angles;
		u32					flags;
		i16					health;
		Entity::Type		weapon;

		enum {
			MaxTouchEnts = 16,
		};
		u16					num_touch_ents;
		u16					touch_ents[MaxTouchEnts];

		void				Update(float dt);
		void				Spawn();
	} g_player;

	////////////////////////////////////////////////////////////////

	constexpr NOINLINE Player::Input GetKeyBinding(int key) {
		switch (key) {
			case Key::W:		return Player::Input::MoveForward;
			case Key::S:		return Player::Input::MoveBack;
			case Key::A:		return Player::Input::MoveLeft;
			case Key::D:		return Player::Input::MoveRight;
			
			case Key::Up:		return Player::Input::MoveForward;
			case Key::Down:		return Player::Input::MoveBack;
			case Key::Left:		return Player::Input::LookLeft;
			case Key::Right:	return Player::Input::LookRight;

			case Key::Space:	return Player::Input::MoveUp;
			case Key::C:		return Player::Input::MoveDown;
			case Key::Ctrl:		return Player::Input::MoveDown;
			
			case Key::PageDown:	return Player::Input::LookUp;
			case Key::Del:		return Player::Input::LookDown;
			case Key::End:		return Player::Input::LookCenter;

			case Key::Shift:	return Player::Input::Run;
			
			case Key::Mouse2:	return Player::Input::Zoom;

			default:			return Player::Input::Invalid;
		}
	}

	constexpr auto KeyBindings =
		MakeLookupTable<int, Player::Input, 0, 255>(GetKeyBinding);

	constexpr auto EntityRespawnTime =
		MakeLookupTable<Entity::Type, u8, Entity::Type{}, Entity::Type::Count>(Entity::GetRespawnTime);
}

////////////////////////////////////////////////////////////////

#include "movement.h"

void Demo::Player::Update(float dt) {
	/* read inputs */
	i32 input_list[(u32)Input::Count];
	MemSet(&input_list);
	for (u32 key = 0; key < 256; ++key) {
		Input command = KeyBindings[key];
		if (IsValid(command) && Sys::IsKeyDown(key))
			input_list[(u16)command] = 1;
	}

#ifdef SHOW_POSITION
	if (Sys::IsKeyFirstDown(Key::P)) {
		Sys::Printf("#define START_POSITION %d, %d, %d\n", int(position.x), int(position.y), int(position.z));
		Sys::Printf("#define START_ANGLES   %d, %d\n", int(angles.x), int(angles.y));
	}
#endif

	bool noclip = NoClipping();
	if (!noclip) {
		if (input_list[(u32)Input::MoveUp]) {
			if (flags & NoJump) {
				// prevent speed loss
				input_list[(u32)Input::MoveUp] = 0;
				input_list[(u32)Input::MoveDown] = 0;
			} else if (ground) {
				ground = nullptr;
				velocity.z = JumpSpeed;
				flags |= NoJump;
			}
		} else {
			flags &= ~NoJump;
		}
	}

	/* initialize movement/look axes */
	union {
		struct {
			vec3 cmd;
			vec2 wishturn;
		};
		float input_axis[5];
	};

	for (u32 i = 0; i < 10; i += 2)
		input_axis[i >> 1] = float(input_list[i] - input_list[i + 1]);

	float run = 1.f - 0.5f * input_list[(u32)Input::Run];

	/* turn with keyboard */
	wishturn *= TurnSpeed * dt * run;
	angles.xy += wishturn;
	if (input_list[(u32)Player::Input::LookCenter])
		angles.y = 0.f;
	angles.x = mod(angles.x, 360.f);
	angles.y = clamp(angles.y, -85.f, 85.f);

	/* movement input */
	float yaw = Math::ToRadians(angles.x);
	float pitch = noclip ? Math::ToRadians(angles.y) : 0.f;
	float sin_pitch = sin(pitch);
	float cos_pitch = cos(pitch);

	vec3 forward, right;
	right.x = sin(yaw);
	right.y = -cos(yaw);
	right.z = 0.f;
	forward.x = -right.y * cos_pitch;
	forward.y = right.x * cos_pitch;
	forward.z = sin_pitch;

	/* apply friction */
	if (ground) {
		ClipVelocity(right, ground->xyz);
		ClipVelocity(forward, ground->xyz);

		float speed = length(velocity.xy);
		if (speed < 1.f) {
			velocity.xy = 0.f;
			//walk_cycle = 0.f;
		} else {
			float drop = max(speed, StopSpeed) * Friction * dt;
			velocity.xy *= max(0.f, speed - drop) / speed;
			walk_cycle += dt * run;
		}
	}

	vec3 wishdir;
	mul(wishdir, forward, cmd.x);
	mad(wishdir, right, cmd.y);

	/* accelerate */
	if (noclip) {
		wishdir.z += cmd.z;
		wishdir *= MoveSpeed * run;
		mix_into(velocity, wishdir, min(1.f, dt * NoClipAccel));
	} else {
		float scale = length(cmd.xy);
		if (scale > 0.f)
			wishdir /= scale;

		float wish_speed = MoveSpeed * run;
		float current_speed = dot(velocity, wishdir);
		float add_speed = wish_speed - current_speed;
		if (add_speed > 0.f) {
			float accel = ground ? GroundAccel : AirAccel;
			float accel_speed = min(add_speed, accel * dt * wish_speed);
			mad(velocity, wishdir, accel_speed);
		}
	}

	/* animate stair-stepping, landing & teleportation */
	step = max(0.f, step - step * dt * 8.f);
	land_time = max(0.f, land_time - dt);
	teleport = max(0.f, teleport - dt / TeleportTime);
	
	/* move */
	float prev_z_speed = velocity.z;
	if (length_squared(velocity.xy) < 1.f)
		MemSet(&velocity.xy);
	else
		shadow_angle = atan2(velocity.y, velocity.x);

	if (noclip) {
		mad(position, velocity, dt);
		ground = nullptr;
		num_touch_ents = 0;
	} else {
		StepSlideMove(*this, dt);
		GroundTrace(*this);
	}

	/* land */
	if (prev_z_speed < -LandSpeed && ground) {
		float severity = prev_z_speed / (-LandSpeed / 8.f);
		if (severity > 24.f)
			severity = 24.f;
		land_change = severity;
		land_time = LandTime;
		walk_cycle = 0.f;
	}

	/* zoom in/out */
	float zoom_delta = dt / ZoomTime;
	if (input_list[(u32)Input::Zoom])
		zoom = min(1.f, zoom + zoom_delta);
	else
		zoom = max(0.f, zoom - zoom_delta);

	/* touch entities */
	for (u16 i = 0; i < num_touch_ents; ++i) {
		u16 entity_index = touch_ents[i];
		Entity& entity = Map::entities[entity_index];
		Entity* target = Map::GetTargetEntity(entity.target);

		switch (entity.type) {
			case Entity::Type::trigger_teleport: {
				if (target) {
					position[0] = target->origin[0];
					position[1] = target->origin[1];
					position[2] = target->origin[2] + SpawnOffset;
					angles[0] = target->angle;
					angles[1] = 0.f;
					angles[2] = 0.f;
					float yaw = Math::ToRadians(angles[0]);
					velocity.x = cos(yaw) * TeleportSpeed;
					velocity.y = sin(yaw) * TeleportSpeed;
					velocity.z = 0.f;
					step = 0.f;
					teleport = 1.f;
				}
				break;
			}

			case Entity::Type::trigger_push: {
				if (target) {
					float height = target->origin[2] - position[2] + SpawnOffset;
					float time = sqrt(height / (0.5f * g_gravity.value));
					velocity[0] = (target->origin[0] - position[0]) / time;
					velocity[1] = (target->origin[1] - position[1]) / time;
					velocity[2] = time * g_gravity.value;
				}
				break;
			}

			case Entity::Type::trigger_hurt: {
				// TODO: handle random, wait
				health -= entity.dmg;
				if (health < 0)
					Spawn();
				break;
			}

			default: {
				if (entity.IsPickup()) {
					if (entity.IsSpawned()) {
						entity.respawn = float(EntityRespawnTime[entity.type]);
					}
				}
				break;
			}
		}
	}
		
	assert(!isnan(position.x));
}

NOINLINE void Demo::Player::Spawn() {
	vec4 spawn_points[Map::MAX_NUM_ENTITIES];
	u16 num_spawn_points = 0;

	for (u16 i = Map::num_brush_entities; i < Map::num_entities; ++i) {
		Entity& e = Map::entities[i];
		if (e.type == Entity::Type::info_player_deathmatch) {
			vec4& spawn = spawn_points[num_spawn_points++];
			spawn[0] = e.origin[0];
			spawn[1] = e.origin[1];
			spawn[2] = e.origin[2] + SpawnOffset;
			spawn[3] = e.angle;
		}
	}

	MemSet(this);

	assert(num_spawn_points > 0);
	u32 index = Random() % num_spawn_points;
	const vec4& spawn = spawn_points[index];

	position	= spawn.xyz;
	angles.x	= spawn.w;
	step		= 16.f;
	health		= 100;
	weapon		= Entity::Type::weapon_rocketlauncher;
	flags		= Flag::NoJump;
	teleport	= 1.f;

#ifdef START_POSITION
	position	= vec3(START_POSITION);
	flags		|= Flag::NoClip;
#endif

#ifdef START_ANGLES
	angles.xy	= vec2(START_ANGLES);
#endif
}
