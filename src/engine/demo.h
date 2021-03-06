#pragma once

#ifndef NDEBUG
	#define DEV
#endif

#ifndef assert
	#ifndef NDEBUG
		namespace Sys { void Log(const char* text); }

		#define assert(condition)											\
			 do {															\
				if (!(condition)) {											\
					Sys::Log("\nAssertion failed:\n\n" #condition "\n\n");	\
					BREAKPOINT();											\
				}															\
			 } while (0)
	#else
		#define assert(condition) ((void)(condition))
	#endif // ndef NDEBUG
#endif

#define NO_CRT

#include "core.h"
#include "math.h"
#include "aabb.h"
using namespace Math::CRT;

//FORCEINLINE void* __cdecl operator new(size_t bytes, void* ptr) noexcept { return ptr; }

////////////////////////////////////////////////////////////////

#include "sys.h"
#include "gfx.h"

#include "sys_win32.h"
#include "gfx_opengl.h"

