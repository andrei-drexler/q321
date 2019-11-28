#pragma once

namespace Demo {
	struct Player {
		static constexpr float
			MoveSpeed		= 320.f,
			StepUpSpeed		= 32.f,
			TurnSpeed		= 90.f,
			TeleportSpeed	= 400.f,
			JumpSpeed		= 270.f,

			LandDeflectTime	= 0.15625f,		// 0.15 in Q3
			LandReturnTime	= 0.3125f,		// 0.3
			LandTime		= LandDeflectTime + LandReturnTime,

			Height			= 56.f,
			HalfHeight		= Height * 0.5f,
			HalfWidth		= 15.f,
			EyeLevel		= 48.f,
			EyeCenterOffset	= EyeLevel - HalfHeight,
			SpawnOffset		= HalfHeight - 4.f
		;

		static constexpr vec3
			CollisionBounds = {HalfWidth, HalfWidth, HalfHeight};

		enum class Input : u8 {
			MoveForward,
			MoveBack,
			MoveLeft,
			MoveRight,
			MoveUp,
			MoveDown,
			LookLeft,
			LookRight,
			LookUp,
			LookDown,
			LookCenter,
			Run,

			Count,
			Invalid = Count,
		};

		enum Flag {
			NoJump		= 1 << 0,
		};

		static constexpr u32 InputMask(Input i)	{
			bool valid = u8(i) < u8(Input::Invalid);
			return valid << (u8)i;
		}

		vec3			position;
		vec3			velocity;
		float			step;
		float			land_change;
		float			land_time;
		const vec4*		ground;
		vec3			angles;
		u32				inputs;
		u32				flags;
		i16				health;

		enum {
			MaxTouchEnts = 16,
		};
		u16				touch_ents[MaxTouchEnts];
		u16				num_touch_ents;

		bool			Has(Input input) const		{ return inputs & InputMask(input); }
		void			Set(Input input)			{ inputs |= InputMask(input); }
		void			Clear(Input input)			{ inputs &= ~InputMask(input); }

		void			Update(const u8* keys, float dt);
		void			Spawn();
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

			default:			return Player::Input::Invalid;
		}
	}

	constexpr auto KeyBindings =
		MakeLookupTable<int, Player::Input, 0, 255>(GetKeyBinding);
}

////////////////////////////////////////////////////////////////

#include "player_move.h"

void Demo::Player::Update(const u8* keys, float dt) {
	inputs = 0;
	for (u16 i = 0; i < 256; ++i)
		if (keys[i])
			Set(KeyBindings[i]);

	if (!(inputs & (InputMask(Input::MoveUp) | InputMask(Input::MoveDown))))
		flags &= ~NoJump;

	float run = 1.f - 0.5f * Has(Input::Run);

	vec2 wishturn;
	wishturn.x = float(Has(Input::LookLeft) - Has(Input::LookRight));
	wishturn.y = float(Has(Input::LookUp) - Has(Input::LookDown));
	wishturn *= TurnSpeed * dt * run;
	angles.xy += wishturn;
	if (Has(Player::Input::LookCenter))
		angles.y = 0.f;
	angles.x = mod(angles.x, 360.f);
	angles.y = clamp(angles.y, -85.f, 85.f);

	float yaw = angles.x * Math::DEG2RAD;
	vec2 forward, side;
	side.x = cos(yaw);
	side.y = sin(yaw);
	forward.x = -side.y;
	forward.y = side.x;

	vec3 wishdir;
	wishdir.y = float(Has(Input::MoveForward) - Has(Input::MoveBack));
	wishdir.x = float(Has(Input::MoveRight) - Has(Input::MoveLeft));
	wishdir.z = float(Has(Input::MoveUp) - Has(Input::MoveDown));

	if (ground && wishdir.z > 0.f && !(flags & NoJump)) {
		ground = nullptr;
		velocity.z += JumpSpeed;
		flags |= NoJump;
	}

	vec2 new_velocity = wishdir.x * side;
	new_velocity += wishdir.y * forward;
	new_velocity *= MoveSpeed * run;

	if (ground) {
		float f = min(1.f, dt * 8.f);
		velocity.x += (new_velocity.x - velocity.x) * f;
		velocity.y += (new_velocity.y - velocity.y) * f;
	} else {
		float f = dt * 0.75f;
		velocity.x += new_velocity.x * f;
		velocity.y += new_velocity.y * f;
		velocity.z -= g_gravity.value * dt;
	}

	step -= step * dt * 8.f;
	if (step < 0.f)
		step = 0.f;

	land_time -= dt;
	if (land_time < 0.f)
		land_time = 0.f;

	float prev_z_speed = velocity.z;
	if (length_squared(velocity) < 1.f)
		velocity = 0.f;
	else
		StepSlideMove(*this, dt);

	if (prev_z_speed < -JumpSpeed && velocity.z > -1.f) {
		float severity = (velocity.z - prev_z_speed) / JumpSpeed;
		if (severity >= 1.f) {
			land_change = severity * 8.f;
			if (land_change > 24.f)
				land_change = 24.f;
			land_time = LandTime;
		}
	}

	for (u16 i = 0; i < num_touch_ents; ++i) {
		u16 entity_index = touch_ents[i];
		Entity& entity = g_map.entities[entity_index];
		Entity* target = g_map.PickTarget(entity.target);

		switch (entity.type) {
			case Entity::Type::trigger_teleport: {
				if (target) {
					position[0] = target->origin[0];
					position[1] = target->origin[1];
					position[2] = target->origin[2] + SpawnOffset;
					angles[0] = target->angle;
					angles[1] = 0.f;
					angles[2] = 0.f;
					velocity.y = cos(angles[0] * -Math::DEG2RAD) * TeleportSpeed;
					velocity.x = sin(angles[0] * -Math::DEG2RAD) * TeleportSpeed;
					velocity.z = 0.f;
					step = 0.f;
				}
				break;
			}

			case Entity::Type::trigger_push: {
				if (target) {
					vec3 target_pos;
					target_pos[0] = target->origin[0];
					target_pos[1] = target->origin[1];
					target_pos[2] = target->origin[2] + SpawnOffset;
					float height = target_pos.z - position.z;
					float time = sqrt(height / (0.5f * g_gravity.value));
					velocity[0] = (target_pos[0] - position[0]) / time;
					velocity[1] = (target_pos[1] - position[1]) / time;
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

			default:
				break;
		}
	}
		
	assert(!isnan(position.x));
}

NOINLINE void Demo::Player::Spawn() {
	vec4 spawn_points[Map::MAX_NUM_ENTITIES];
	u16 num_spawn_points = 0;

	for (u16 i = g_map.num_brush_entities; i < g_map.num_entities; ++i) {
		auto& e = g_map.entities[i];
		if (e.type == Entity::Type::info_player_deathmatch) {
			auto& spawn = spawn_points[num_spawn_points++];
			spawn[0] = e.origin[0];
			spawn[1] = e.origin[1];
			spawn[2] = e.origin[2] + SpawnOffset;
			spawn[3] = e.angle;
		}
	}

	assert(num_spawn_points > 0);
	u32 index = Random() % num_spawn_points;
	auto& spawn = spawn_points[index];

	position = spawn.xyz;
	angles.x = spawn.w;
	angles.y = 0.f;
	angles.z = 0.f;
	velocity = 0.f;
	step = 16.f;
	land_time = 0.f;
	health = 100;
	flags = Flag::NoJump;
}
