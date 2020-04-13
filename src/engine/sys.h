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

	[[noreturn]] void			Fatal(int code);
	[[noreturn]] void			Exit(int code = 0);
	
	void						Log(const char* text);
	void						DebugLog(const char* text);

	void						PrintfEx(const char* format, const void* const* args, size_t count);

	template <typename... Args>
	FORCEINLINE void			Printf(const char* format, const Args&... args) { const void* list[] = {&args...}; PrintfEx(format, list, sizeof...(Args)); }
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

	struct Point {
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

	void GetKeyboardState(u8* state);
	
	bool PumpMessages(int* error = nullptr);
	int RunApplication();

	////////////////////////////////////////////////////////////////

	struct Thread {
		void*	handle;
		void	(*work)(void* data);
		void*	data;
	};
	
	u32			GetNumCPUThreads();
	void		Sleep(float duration);
	void		SpawnThread(Thread& thread);
	void		JoinThread(Thread& thread);
	bool		IsThreadReady(const Thread& thread);

	////////////////////////////////////////////////////////////////

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

	////////////////////////////////////////////////////////////////

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
			u8			box_size[2];
			i8			anchor[2];
			u8			advance;
		};
	}

	void RasterizeFont(const char* name, int font_size, u32 flags, u32* pixels, u16 width, u16 height, RectPacker& packer, Font::Glyph* glyphs);
}

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

NOINLINE void Sys::PrintfEx(const char* format, const void* const* args, size_t count) {
	const size_t BufferSize = 4096;
	char buffer[BufferSize];

	auto next_arg = [&] () {
		assert(count--);
		return *args++;
	};

	#define ARG_PTR(type)		((type const*)next_arg())
	#define ARG(type)			(*ARG_PTR(type))

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
				IntToString(ARG(i32), buffer);
				Log(buffer);
			} else if (*format == 'f') {
				FloatToString(ARG(float), buffer);
				Log(buffer);
			} else if (*format == 's') {
				Log(ARG_PTR(char));
			} else if (*format == '%') {
				Log("%");
			} else if (*format == '\0') {
				Log("%");
				break;
			}
			++format;
		}
	}

	#undef ARG
	#undef ARG_PTR
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
		size = (size + (ALIGNMENT - 1)) & ~(ALIGNMENT - 1);
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
}

////////////////////////////////////////////////////////////////

#include "rect_packer.h"
