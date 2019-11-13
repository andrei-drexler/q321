#pragma once

namespace Demo {
	struct Player {
		static constexpr float
			MoveSpeed		= 320.f,
			StepUpSpeed		= 32.f,
			TurnSpeed		= 90.f,

			Height			= 56.f,
			HalfHeight		= Height * 0.5f,
			HalfWidth		= 15.f,
			EyeLevel		= 48.f,
			EyeCenterOffset	= EyeLevel - HalfHeight
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

		static constexpr u32 InputMask(Input i)	{
			bool valid = u8(i) < u8(Input::Invalid);
			return valid << (u8)i;
		}

		vec3			position;
		vec3			velocity;
		float			step;
		const vec4*		ground;
		vec3			angles;
		u32				inputs;

		mat4			orientation;

		bool			Has(Input input) const		{ return inputs & InputMask(input); }
		void			Set(Input input)			{ inputs |= InputMask(input); }
		void			Clear(Input input)			{ inputs &= ~InputMask(input); }

		void			Update(float dt);
	} g_player;
}

////////////////////////////////////////////////////////////////

#include "player_move.h"

void Demo::Player::Update(float dt) {
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

	orientation = MakeRotation(angles * Math::DEG2RAD);

	vec3 wishdir;
	wishdir.y = float(Has(Input::MoveForward) - Has(Input::MoveBack));
	wishdir.x = float(Has(Input::MoveRight) - Has(Input::MoveLeft));
	wishdir.z = float(Has(Input::MoveUp) - Has(Input::MoveDown));

	vec3 new_velocity = wishdir.x * orientation[0].xyz;
	new_velocity += wishdir.y * orientation[1].xyz;
	new_velocity.z += wishdir.z;
	new_velocity *= MoveSpeed * run;

	mix_into(velocity, new_velocity, min(1.f, dt * 8.f));

	step -= step * dt * 8.f;
	if (step < 0.f)
		step = 0.f;

	if (length_squared(velocity) < 1.f)
		velocity = 0.f;
	else
		//SlideMove(*this, dt);
		StepSlideMove(*this, dt);
		
	assert(!isnan(position.x));
}
