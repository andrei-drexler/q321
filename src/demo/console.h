#pragma once

namespace Demo {
	struct Cvar {
		i32 integer;
		f32 value;
	};

	////////////////////////////////////////////////////////////////

	#define CVAR_LIST(x)					\
		x(sensitivity,		1)				\
		x(com_maxFps,		0)				\
		x(cl_fov,			90)				\
		x(r_lightmap,		0)				\
		x(g_gravity,		800)			\

	////////////////////////////////////////////////////////////////
	
	namespace Console {
		#define PP_CVAR_ADD_NAME(name, init)		#name "\0"
		#define PP_CVAR_ADD_INIT(name, init)		#init "\0"
		#define PP_CVAR_COUNT(name, init)			+1
		
		static constexpr char	CvarNames[]			= CVAR_LIST(PP_CVAR_ADD_NAME);
		static constexpr char	CvarInit[]			= CVAR_LIST(PP_CVAR_ADD_INIT);
		enum { NumCvars = CVAR_LIST(PP_CVAR_COUNT) };

		#undef PP_CVAR_ADD_NAME
		#undef PP_CVAR_ADD_INIT
		#undef PP_CVAR_COUNT

		void Init();
	}

	////////////////////////////////////////////////////////////////

	static union {
		struct {
			#define PP_CVAR_ADD_DATA(name, init)	Cvar name;
			CVAR_LIST(PP_CVAR_ADD_DATA)
			#undef PP_CVAR_ADD_DATA
		};
		Cvar CvarData[Console::NumCvars];
	};
}

////////////////////////////////////////////////////////////////

NOINLINE float ParseFloat(const char* text, iptr* consumed = 0) {
	const char* start = text;
	while (*text == ' ' || *text == '\t')
		++text;
	iptr spaces = text - start;

	char c = *text;
	bool negative = (c == '-');
	bool has_sign = ((c == '+') | negative);
	text += has_sign;
	i32 i = 0;
	while (u8(*text - '0') < 10)
		i = i * 10 + (*text++ - '0');

	float result = float(i);
	bool has_dot = (*text == '.');
	if (has_dot) {
		++text;
		i = 0;
		float denom = 1.f;
		while (u8(*text - '0') < 10) {
			i = i * 10 + (*text++ - '0');
			denom *= 10.f;
		}
		result += i / denom;
	}

	if (consumed) {
		iptr dist = text - start;
		*consumed = (dist > spaces + has_sign + has_dot) ? dist : 0;
	}

	return negative ? -result : result;
}

FORCEINLINE int ftoi(float f) {
	float posf = f < 0.f ? -f : f;
	int i = (int)posf;
	i -= float(i) > posf;
	return f < 0.f ? -i : i;
}

FORCEINLINE void Demo::Console::Init() {
	const char* value = CvarInit;
	for (u32 cvar_index = 0; cvar_index < NumCvars; ++cvar_index) {
		auto& cvar = CvarData[cvar_index];
		iptr advance = 0;
		cvar.value = ParseFloat(value, &advance);
		cvar.integer = ftoi(cvar.value);
		assert(advance > 0 && value[advance] == 0);
		value += advance;
		++value;
	}
}
