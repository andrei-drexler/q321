#pragma once

#define DEMO_ENTITY_PROPERTIES(x)	\
	x(origin,			i16[3])		\
	x(light,			i16)		\
	x(spawnflags,		i16)		\
	x(target,			i16)		\
	x(targetname,		i16)		\
	x(angle,			i16)		\
	/*x(noise,			i16[3])*/	\
	/*x(model,			i16[3])*/	\
	x(_color,			i16)		\
	x(wait,				i16)		\
	x(random,			i16)		\
	/*x(music,			i16[3])*/	\
	/*x(message,		i16[3])*/	\
	/*x(ambient,		i16[3])*/	\
	x(dmg,				i16)		\

#define DEMO_ENTITY_TYPES(x)		\
	x(worldspawn)					\
	x(trigger_teleport)				\
	x(trigger_multiple)				\
	x(trigger_hurt)					\
	/*x(light)*/					\
	x(target_position)				\
	/*x(target_speaker)*/			\
	x(trigger_push)					\
	x(info_player_deathmatch)		\
	x(item_armor_shard)				\
	x(item_health_large)			\
	x(misc_model)					\
	x(ammo_bullets)					\
	x(info_player_intermission)		\
	x(ammo_rockets)					\
	x(ammo_shells)					\
	x(ammo_slugs)					\
	x(weapon_rocketlauncher)		\
	x(weapon_shotgun)				\
	x(item_health)					\
	x(item_armor_body)				\
	x(target_remove_powerups)		\
	x(item_armor_combat)			\
	x(item_health_mega)				\
	x(weapon_railgun)				\
	x(misc_teleporter_dest)			\
	x(item_quad)					\

////////////////////////////////////////////////////////////////

namespace Demo {
	struct Entity {
		enum class Type {
			None,

			#define PP_DEMO_ENTITY_TYPE_DECLARE(name)			name,
			DEMO_ENTITY_TYPES(PP_DEMO_ENTITY_TYPE_DECLARE)
			#undef PP_DEMO_ENTITY_TYPE_DECLARE

			Count,
		} type;

		// since 'i16[3] origin;' is invalid, we use helper aliases
		#define PP_DEMO_PROP_DECLARE(name, type)			using typeof_##name = type; typeof_##name name;
		DEMO_ENTITY_PROPERTIES(PP_DEMO_PROP_DECLARE)
		#undef PP_DEMO_PROP_DECLARE
	};
}

