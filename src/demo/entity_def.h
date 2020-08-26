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

////////////////////////////////////////////////////////////////

#define DEMO_ENTITY_TYPES_WEAPONS(x)\
	/*Name							Description				Model								  R,   G,   B*/\
	x(weapon_gauntlet,				"Gauntlet",				Demo::Model::gauntlet_barrel,		  0,   0,   0)\
	x(weapon_machinegun,			"Machinegun",			Demo::Model::machinegun_barrel,		  0,   0,   0)\
	x(weapon_shotgun,				"Shotgun",				Demo::Model::shotgun,				  0,   0,   0)\
	x(weapon_rocketlauncher,		"Rocket launcher",		Demo::Model::rocketl,				  0,   0,   0)\
	x(weapon_railgun,				"Railgun",				Demo::Model::railgun,				  0,   0,   0)\
	x(weapon_plasmagun,				"Plasma Gun",			Demo::Model::plasma,				  0,   0,   0)\

#define DEMO_ENTITY_TYPES_AMMO(x)\
	/*Name							Description				Model								  R,   G,   B*/\
	x(ammo_bullets,					"Bullets",				Demo::Model::rocketam,				128, 112,   2)\
	x(ammo_rockets,					"Rockets",				Demo::Model::rocketam,				128,   2,   2)\
	x(ammo_shells,					"Shells",				Demo::Model::rocketam,				128,  64,   2)\
	x(ammo_slugs,					"Slugs",				Demo::Model::rocketam,				  0,  80,  32)\
	x(ammo_cells,					"Cells",				Demo::Model::rocketam,				112,   0,  64)\

#define DEMO_ENTITY_TYPES_HEALTH(x)\
	/*Name							Description				Model								  R,   G,   B*/\
	x(item_health,					"25 Health",			Demo::Model::large_cross,			160, 128,  48)\
	x(item_health_large,			"50 Health",			Demo::Model::large_cross,			144,  88,  56)\
	x(item_health_mega,				"Mega Health",			Demo::Model::mega_cross,			  8, 119, 234)\

#define DEMO_ENTITY_TYPES_ARMOR(x)\
	/*Name							Description				Model								  R,   G,   B*/\
	x(item_armor_combat,			"Armor",				Demo::Model::armor_red,				255, 255,   0)\
	x(item_armor_shard,				"Armor Shard",			Demo::Model::shard,					 80,  88,  86)\
	x(item_armor_body,				"Heavy Armor",			Demo::Model::armor_red,				255,   0,  16)\

#define DEMO_ENTITY_TYPES_POWERUPS(x)\
	/*Name							Description				Model								  R,   G,   B*/\
	x(item_quad,					"Quad Damage",			Demo::Model::quad,					  8, 119, 234)\

#define DEMO_ENTITY_TYPES_PICKUPS(x)\
	DEMO_ENTITY_TYPES_WEAPONS(x)\
	DEMO_ENTITY_TYPES_AMMO(x)\
	DEMO_ENTITY_TYPES_HEALTH(x)\
	DEMO_ENTITY_TYPES_ARMOR(x)\
	DEMO_ENTITY_TYPES_POWERUPS(x)\

#define DEMO_ENTITY_TYPES(x)		\
	/*Name							Description				Model								  R,   G,   B*/\
	x(worldspawn,					"",						-1,									  0,   0,   0)\
	x(trigger_teleport,				"",						-1,									  0,   0,   0)\
	x(trigger_multiple,				"",						-1,									  0,   0,   0)\
	x(trigger_hurt,					"",						-1,									  0,   0,   0)\
	x(trigger_push,					"",						-1,									  0,   0,   0)\
	/*x(light,						"",						-1,									  0,   0,   0)*/\
	x(target_position,				"",						-1,									  0,   0,   0)\
	/*x(target_speaker,				"",						-1,									  0,   0,   0)*/\
	x(misc_model,					"",						-1,									  0,   0,   0)\
	x(info_player_deathmatch,		"",						-1,									  0,   0,   0)\
	x(info_player_intermission,		"",						-1,									  0,   0,   0)\
	x(misc_teleporter_dest,			"",						-1,									  0,   0,   0)\
	DEMO_ENTITY_TYPES_PICKUPS(x)\
	x(target_remove_powerups,		"",						-1,									  0,   0,   0)\

////////////////////////////////////////////////////////////////

namespace Demo {
	struct BaseEntity { // Only contains properties read from the map file
		enum class Type : i16 {
			#define PP_DEMO_ENTITY_TYPE_DECLARE(name,...)	name,

			DEMO_ENTITY_TYPES(PP_DEMO_ENTITY_TYPE_DECLARE)

			Count,

			#define PP_DEMO_ENTITY_TYPE_COUNT(name,...)		+1
			#define PP_DEMO_ENTITY_TYPE_LIST_RANGE(name,list)					\
				name##Start = PP_FIRST_ARG(list(PP_DEMO_ENTITY_TYPE_DECLARE)),	\
				name##Count = list(PP_DEMO_ENTITY_TYPE_COUNT)					\

			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Weapon,  DEMO_ENTITY_TYPES_WEAPONS),
			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Ammo,    DEMO_ENTITY_TYPES_AMMO),
			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Health,  DEMO_ENTITY_TYPES_HEALTH),
			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Armor,   DEMO_ENTITY_TYPES_ARMOR),
			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Powerup, DEMO_ENTITY_TYPES_POWERUPS),
			PP_DEMO_ENTITY_TYPE_LIST_RANGE(Pickup,  DEMO_ENTITY_TYPES_PICKUPS),

			#undef PP_DEMO_ENTITY_TYPE_DECLARE
			#undef PP_DEMO_ENTITY_TYPE_COUNT
			#undef PP_DEMO_ENTITY_TYPE_LIST_FIRST
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

		#define PP_ADD_ENTITY_TYPE_CHECK(name)																								\
			static constexpr bool	Is##name(Type type)		{ return u8(u32(type) - u32(Type::name##Start)) < u8(Type::name##Count); }		\
			constexpr bool			Is##name() const		{ return Is##name(type); }														\

		PP_ADD_ENTITY_TYPE_CHECK(Weapon)
		PP_ADD_ENTITY_TYPE_CHECK(Ammo)
		PP_ADD_ENTITY_TYPE_CHECK(Health)
		PP_ADD_ENTITY_TYPE_CHECK(Armor)
		PP_ADD_ENTITY_TYPE_CHECK(Powerup)
		PP_ADD_ENTITY_TYPE_CHECK(Pickup)

		#undef PP_ADD_ENTITY_TYPE_CHECK

		////////////////////////////////////////////////////////////////

		static constexpr u8 GetRespawnTime(Type type) {
			if (IsWeapon(type))		return 5;
			if (IsAmmo(type))		return 40;
			if (IsHealth(type))		return 35;
			if (IsArmor(type))		return 25;
			if (IsPowerup(type))	return 120;

			return 5;
		}
	};

	////////////////////////////////////////////////////////////////

	struct alignas(16) Entity : BaseEntity { // Adds data needed at runtime
		static constexpr float
			Radius				= 15.f,
			RespawnAnimTime		= 0.5f
		;

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

		float respawn;
		bool IsSpawned() const { return respawn < RespawnAnimTime; }
	};
}

