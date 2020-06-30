#pragma once

#define DEMO_ENTITY_PROPERTIES(x)	\
	x(origin,			i16[3])		\
	x(light,			i16)		\
	x(spawnflags,		i16)		\
	x(target,			i16)		\
	x(targetname,		i16)		\
	x(angle,			i16)		\
	/*x(noise,			i16[3])*/	\
	x(model,			i16)		\
	x(_color,			i16)		\
	x(wait,				i16)		\
	x(random,			i16)		\
	/*x(music,			i16[3])*/	\
	/*x(message,		i16[3])*/	\
	/*x(ambient,		i16[3])*/	\
	x(dmg,				i16)		\

#define DEMO_ENTITY_TYPES(x)		\
	x(worldspawn,					"")\
	x(trigger_teleport,				"")\
	x(trigger_multiple,				"")\
	x(trigger_hurt,					"")\
	x(trigger_push,					"")\
	/*x(light,						"")*/\
	x(target_position,				"")\
	/*x(target_speaker,				"")*/\
	x(misc_model,					"")\
	x(info_player_deathmatch,		"")\
	x(info_player_intermission,		"")\
	x(misc_teleporter_dest,			"")\
	x(ammo_bullets,					"Bullets")\
	x(ammo_rockets,					"Rockets")\
	x(ammo_shells,					"Shells")\
	x(ammo_slugs,					"Slugs")\
	x(ammo_cells,					"Cells")\
	x(weapon_rocketlauncher,		"Rocket launcher")\
	x(weapon_shotgun,				"Shotgun")\
	x(weapon_railgun,				"Railgun")\
	x(weapon_plasmagun,				"Plasma Gun")\
	x(item_health,					"25 Health")\
	x(item_health_large,			"50 Health")\
	x(item_health_mega,				"Mega Health")\
	x(item_armor_combat,			"Armor")\
	x(item_armor_shard,				"Armor Shard")\
	x(item_armor_body,				"Heavy Armor")\
	x(item_quad,					"Quad Damage")\
	x(target_remove_powerups,		"")\

////////////////////////////////////////////////////////////////

namespace Demo {
	struct BaseEntity { // Only contains properties read from the map file
		enum class Type : i16 {
			None,

			#define PP_DEMO_ENTITY_TYPE_DECLARE(name,...)	name,
			DEMO_ENTITY_TYPES(PP_DEMO_ENTITY_TYPE_DECLARE)
			#undef PP_DEMO_ENTITY_TYPE_DECLARE

			Count,
		} type;

		enum {
			#define PP_DEMO_PROP_COUNT(name, type)			+sizeof(type)
			NumRawFields = (DEMO_ENTITY_PROPERTIES(PP_DEMO_PROP_COUNT)) / sizeof(i16) + 1, // +1 for entity type
			#undef PP_DEMO_PROP_COUNT
		};

		////////////////////////////////////////////////////////////////

		// since 'i16[3] origin;' is invalid, we use a helper alias
		template <typename T>
		using Field = T;

		#define PP_DEMO_PROP_DECLARE(name, type)			Field<type> name;
		DEMO_ENTITY_PROPERTIES(PP_DEMO_PROP_DECLARE)
		#undef PP_DEMO_PROP_DECLARE

		////////////////////////////////////////////////////////////////

		static constexpr u32 WeaponTypeMask =
			(1 << (u32)Type::weapon_railgun) |
			(1 << (u32)Type::weapon_shotgun) |
			(1 << (u32)Type::weapon_rocketlauncher) |
			(1 << (u32)Type::weapon_plasmagun)
		;

		static constexpr bool IsWeapon(Type type) {
			static_assert(u32(Type::Count) <= 32, "Handle entity types >= 32");

			return 0 != (WeaponTypeMask & (1 << u32(type)));
		}

		constexpr bool IsWeapon() const {
			return IsWeapon(type);
		}
	};

	////////////////////////////////////////////////////////////////

	struct alignas(16) Entity : BaseEntity { // Adds data needed at runtime
		enum : u32 {
			Version = Hash(
				#define PP_DEMO_HASH_ENTITY_TYPE(name, ...)			#name "*"
				#define PP_DEMO_HASH_ENTITY_FIELD(name, type)		#name "=" #type ";"

				DEMO_ENTITY_TYPES(PP_DEMO_HASH_ENTITY_TYPE)
				"/"
				DEMO_ENTITY_PROPERTIES(PP_DEMO_HASH_ENTITY_FIELD)

				#undef PP_DEMO_HASH_ENTITY_FIELD
				#undef PP_DEMO_HASH_ENTITY_TYPE
			)
		};
	};
}

