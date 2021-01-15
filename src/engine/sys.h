#pragma once

////////////////////////////////////////////////////////////////

extern int demo_main();

struct RectPacker;

namespace Sys {
	using Time = double;
	Time						GetTime();

	void*						Alloc(size_t size);
	template <typename T>
	T*							Alloc(size_t count) { return (T*)Alloc(sizeof(T) * count); }
	void						Free(void* alloc);

	using atomic_int			= volatile i32;
	i32							AtomicLoad(atomic_int* addr);
	void						AtomicStore(atomic_int* addr, i32 value);
	i32							AtomicInc(atomic_int* addr);
	i32							AtomicExchangeAdd(atomic_int* addr, i32 value);

	[[noreturn]] void			Fatal(int code);
	[[noreturn]] void			Exit(int code = 0);

	void						Log(const char* text);
	void						DebugLog(const char* text);

	namespace Details {
		union PrintArg {
			int i;
			float f;
			const char* s;

			PrintArg(int i) : i(i) { }
			PrintArg(float f) : f(f) { }
			PrintArg(const char* s) : s(s) { }
		};
	}

	void						PrintfEx(const char* format, const Details::PrintArg* args);

	template <typename... Args>
	FORCEINLINE void			Printf(const char* format, const Args&... args) { const Details::PrintArg list[] = {args...}; PrintfEx(format, list); }
	FORCEINLINE void			Printf(const char* format) { Log(format); }

	void						Breakpoint();

	namespace details {
		struct LogStream		{ FORCEINLINE const LogStream&			operator<<(const char* text) const { Log(text);		 return *this; } };
		struct DebugLogStream	{ FORCEINLINE const DebugLogStream&		operator<<(const char* text) const { DebugLog(text); return *this; } };
	}

	static constexpr details::LogStream			Stream;
	static constexpr details::DebugLogStream	DebugStream;

	////////////////////////////////////////////////////////////////

	enum class MouseButton : i8 {
		Unknown = -1,
		Left,
		Right,
		Middle,
	};

	struct alignas(u32) Point {
		i16 x, y;
	};

	struct Window {
		struct Event {
			Window* window;
			
			enum Type : i8 {
				Unknown = -1,
				Paint,
				Resize,
				
				MouseMove,
				MouseDown,
				MouseUp,

				KeyDown,
				KeyUp,

				Count,
			} type;
		
			union Data {
				struct { i16 width, height;				} resize;
				struct { Point pt;						} mouse_move;
				struct { Point pt; MouseButton button;	} mouse_button, mouse_down, mouse_up;
				struct { i32 code;						} key, key_down, key_up;
			} data;

			using Handler = void (*)(Event&);
		};

		enum Flags {
			Active			= 1 << 0,
			FPSMode			= 1 << 1,
		};

		Event::Handler				on_event;
		void*						handle;
		void*						render_context;

		const char*					title;
		i16							width;
		i16							height;
		i16							refresh;
		i16							flags;
	};

	Window g_window;

	void InitWindow(Window* window, Window::Event::Handler handler, const char* name = "Demo");
	void SetFPSMode(Window* window, bool enabled = true);
	void RedrawWindow(Window* window);
	void SetWindowIcon(Window* window, const u32* pixels, u16 size);

	bool PumpMessages(int* error = nullptr);
	int RunApplication();

	// Mouse and keyboard //////////////////////////////////////////

	bool IsKeyDown(u8 key);
	bool IsKeyFirstDown(u8 key);
	bool IsKeyReleased(u8 key);
	bool IsKeyToggled(u8 key, bool new_state = true);
	bool IsKeyRepeating(u8 key); // returns true for the initial press, too!
	u8 IsAnyKeyFirstDown();

	void UpdateKeyboardState();
	void UpdateMouseState(vec2& pt, float dt);

	// Threading ///////////////////////////////////////////////////

	struct Thread {
		void*	handle;
		void	(*work)(void* data);
		void*	data;
	};
	
	u32			GetNumCPUThreads();
	void		Sleep(float seconds);
	void		PreciseSleep(float seconds); // for main thread only
	void		SpawnThread(Thread& thread);
	void		JoinThread(Thread& thread);
	bool		IsThreadReady(const Thread& thread);

	// File system /////////////////////////////////////////////////

	namespace File {
		enum Mode {
			Read,
			Write,
		};

		struct Handle {
			void* data;

			explicit operator bool() const;
		};
	}

	File::Handle	OpenFile(const char* path, File::Mode mode = File::Mode::Read);
	bool			IsOpen(File::Handle file);
	void			CloseFile(File::Handle& file);
	bool			FileExists(const char* path);
	bool			ReadFromFile(File::Handle file, void* buffer, u32 size, u32* read = nullptr);
	bool			WriteToFile(File::Handle file, const void* buffer, u32 size, u32* written = nullptr);
	u64				GetFileTime(const char* path);
	bool			CreateFolder(const char* name);

	// Font ////////////////////////////////////////////////////////

	namespace Font {
		enum Flags : u8 {
			Normal		= 0,
			Bold		= 1 << 0,
			Italic		= 1 << 1,
		};

		struct alignas(u32) Glyph {
			enum : u8 {
				Begin	= 32,
				End		= 128,
				Count	= End - Begin,
			};

			u16			box_min[2];
			u16			box_size[2];
			i8			anchor[2];
			u8			advance;
		};
	}

	void RasterizeFont(const char* name, int font_size, u32 flags, u32* pixels, u16 width, u16 height, u16 padding, RectPacker& packer, Font::Glyph* glyphs);

	// Dynamic libraries ///////////////////////////////////////////

	struct Library {
		using Function = void (*)();
		
		void* handle;
		explicit operator bool() const { return handle != nullptr; }
	};

	Library				LoadDynamicLibrary(const char* name);
	void				UnloadDynamicLibrary(Library& lib);
	Library::Function	GetRawFunction(Library lib, const char* name);

	template <typename Return, typename... Args>
	FORCEINLINE bool GetFunction(Library lib, const char* name, Return (__cdecl *&out)(Args...)) {
		out = reinterpret_cast<Return(__cdecl*)(Args...)>(GetRawFunction(lib, name));
		return out != nullptr;
	}
}

////////////////////////////////////////////////////////////////
// Implementation //////////////////////////////////////////////
////////////////////////////////////////////////////////////////

Sys::File::Handle::operator bool() const {
	return IsOpen(*this);
}

bool Sys::FileExists(const char* path) {
	File::Handle handle = OpenFile(path);
	if (!handle)
		return false;
	CloseFile(handle);
	return true;
}

NOINLINE void Sys::PrintfEx(const char* format, const Details::PrintArg* args) {
	const size_t BufferSize = 4096;
	char buffer[BufferSize];

	#define NEXT_ARG() (args++)

	size_t write_cursor = 0;
	while (*format) {
		while (*format && write_cursor < BufferSize - 1 && *format != '%')
			buffer[write_cursor++] = *format++;

		if (write_cursor > 0) {
			buffer[write_cursor] = '\0';
			write_cursor = 0;
			Log(buffer);
		}
		
		if (*format == '%') {
			++format;
			if (*format == 'd') {
				IntToString(NEXT_ARG()->i, buffer);
				Log(buffer);
			} else if (*format == 'f') {
				FloatToString(NEXT_ARG()->f, buffer);
				Log(buffer);
			} else if (*format == 's') {
				Log(NEXT_ARG()->s);
			} else if (*format == '%') {
				Log("%");
			} else if (*format == '\0') {
				Log("%");
				break;
			}
			++format;
		}
	}
}

////////////////////////////////////////////////////////////////

namespace Error {
	enum Code {
		Success = 0,

		OutOfMemory,

		BadPixelFormat,
		RenderContext,
		RenderVersion,
		RenderFunctions,
		Shader,
		RenderTarget,

		Font,
		Atlas,

		Startup,

		AssertionFailed,
	};
}

////////////////////////////////////////////////////////////////

namespace Mem {
	constexpr u32
		KB			= 1024,
		MB			= KB * 1024,
		ALIGNMENT	= 16
	;

	struct Arena {
		u8*		buffer;
		u32		low;
		u32		high;
		u32		size;
	};

	Arena g_main_arena;

	FORCEINLINE u32 Aligned(u32 size) {
		return (size + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1);
	}

	FORCEINLINE void Init(Arena* arena = &g_main_arena, u32 size = 128 * MB) {
		arena->buffer	= (u8*)Sys::Alloc(size);
		arena->low		= 0;
		arena->high		= size;
		arena->size		= size;
	}

	FORCEINLINE void Clear(Arena* arena) {
		arena->low = 0;
		arena->high = arena->size;
	}

	NOINLINE void* Alloc(u32 size, Arena* arena = &g_main_arena) {
		size = Aligned(size);
		if (arena->low + size <= arena->high) {
			arena->high -= size;
			return arena->buffer + arena->high;
		}
		Sys::Fatal(Error::OutOfMemory);
		return nullptr;
	}

	template <typename T>
	FORCEINLINE T* Alloc(u32 count = 1, Arena* arena = &g_main_arena) {
		return (T*)Alloc(count * sizeof(T), arena);
	}

	namespace Details {
		struct Header {
			u32		low;
#ifdef DEV
			Arena*	arena;
			u32		size;
#endif
		};
	}

	NOINLINE void* TempAlloc(u32 size, Arena* arena = &g_main_arena) {
		u32 aligned_size = Aligned(size);
		u32 total_size = aligned_size + sizeof(Details::Header);
		if (arena->low + total_size <= arena->high) {
			u8* ptr = arena->buffer + arena->low;
			auto& header = *(Details::Header*)ptr;
			header.low = arena->low;
#ifdef DEV
			header.arena = arena;
			header.size = size;
#endif
			arena->low += total_size;
			return ptr + sizeof(header);
		}
		Sys::Fatal(Error::OutOfMemory);
		return nullptr;
	}

	void TempFree(void* block, Arena* arena = &g_main_arena) {
		assert(uptr(block) - uptr(arena->buffer) < arena->size);
		auto& header = ((Details::Header*)block)[-1];
#ifdef DEV
		assert(header.arena = arena);
#endif
		arena->low = header.low;
	}

	template <typename T>
	FORCEINLINE T* TempAlloc(u32 count = 1, Arena* arena = &g_main_arena) {
		return (T*)TempAlloc(count * sizeof(T), arena);
	}
}

////////////////////////////////////////////////////////////////

#include "rect_packer.h"
