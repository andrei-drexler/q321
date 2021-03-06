#pragma once

////////////////////////////////////////////////////////////////

#ifdef ENABLE_RADIOSITY
	#define PP_RADIOSITY_CVAR(x)	x(r_bounce,			0)
#else
	#define PP_RADIOSITY_CVAR(x)
#endif

#define CVAR_LIST(x)			\
	x(sensitivity,		1)		\
	x(cl_inverty,		0)		\
	x(com_maxFps,		0)		\
	x(cg_fov,			90)		\
	x(cg_zoomfov,		45)		\
	x(r_gamma,			1)		\
	x(r_lightmap,		0)		\
	x(r_fullbright,		0)		\
	PP_RADIOSITY_CVAR(x)		\
	x(g_gravity,		800)	\

////////////////////////////////////////////////////////////////

namespace Demo {
	struct Cvar {
		f32 value;

		enum ID {
			#define PP_CVAR_ADD_NAME(name, init)		name,
			CVAR_LIST(PP_CVAR_ADD_NAME)
			#undef PP_CVAR_ADD_NAME

			Count,
		};

		FORCEINLINE void Set(float new_value) {
			value = new_value;
		}

		NOINLINE void Toggle() {
			Set(!value);
		}
	};

	////////////////////////////////////////////////////////////////

	namespace Console {
		#define PP_CVAR_ADD_NAME(name, init)		#name "\0"
		#define PP_CVAR_ADD_INIT(name, init)		#init "\0"

		static constexpr char	CvarNames[]			= CVAR_LIST(PP_CVAR_ADD_NAME);
		static constexpr char	CvarInit[]			= CVAR_LIST(PP_CVAR_ADD_INIT);

		#undef PP_CVAR_ADD_NAME
		#undef PP_CVAR_ADD_INIT

		void Init();
	}

	////////////////////////////////////////////////////////////////

	static union {
		struct {
			#define PP_CVAR_ADD_DATA(name, init)	Cvar name;
			CVAR_LIST(PP_CVAR_ADD_DATA)
			#undef PP_CVAR_ADD_DATA
		};
		Cvar CvarData[Cvar::Count];
	};
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Console::Init() {
	const char* value = CvarInit;
	for (u32 cvar_index = 0; cvar_index < Cvar::Count; ++cvar_index, value = NextAfter(value)) {
		Cvar& cvar = CvarData[cvar_index];
		cvar.Set(StringToFloat(value));
	}
}
