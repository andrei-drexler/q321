#pragma once

#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#define NOMINMAX
#define _WIN32_WINNT 0x0603
#include <Windows.h>

#include "keys_win32.h"

////////////////////////////////////////////////////////////////

extern "C" void* __cdecl memset(void *dest, int c, size_t count);
extern "C" void* __cdecl memcpy(void *dest, const void *src, size_t count);
extern "C" void* __cdecl memmove(void *dest, const void *src, size_t count);
extern "C" size_t __cdecl strlen(const char* str);

#ifdef _MSC_VER
	#pragma intrinsic(memset)
	#pragma intrinsic(memcpy)
	#pragma intrinsic(memmove)
	#pragma intrinsic(strlen)

	// Put .CRT data into .rdata section
	#pragma comment(linker, "/merge:.CRT=.rdata")

	#pragma comment(lib, "user32.lib")
	#pragma comment(lib, "kernel32.lib")

	extern "C" {
		int _fltused = 0;
		size_t __security_cookie = 0x12345678;

		#pragma function(memset)
		void* __cdecl memset(void* dest, int c, size_t count) {
			char* bytes = (char*)dest;
			while (count--)
				*bytes++ = (char)c;
			return dest;
		}

		#pragma function(memcpy)
		void* __cdecl memcpy(void* dest, const void* src, size_t count) {
			char* dest_bytes = (char*)dest;
			const char* src_bytes = (const char*)src;
			while (count--)
				*dest_bytes++ = *src_bytes++;
			return dest;
		}

		#pragma function(memmove)
		void* __cdecl memmove(void* dest, const void* src, size_t count) {
			char* dest_bytes = (char*)dest;
			const char* src_bytes = (const char*)src;
			while (count--)
				dest_bytes[count] = src_bytes[count];
			return dest;
		}
	}
#else
	#error Unsupported platform
#endif

////////////////////////////////////////////////////////////////

namespace Win32 {
	LARGE_INTEGER g_start_time;
	double g_rcp_frequency;
	HANDLE g_waitable_timer;

	// Used by raw input
	i32 g_mouse_x;
	i32 g_mouse_y;
	i32 g_last_mouse_x;
	i32 g_last_mouse_y;

	WPARAM g_repeat_key;
	bool g_repeat_flag;

	Sys::Point GetPoint(LPARAM lParam) {
		return { SHORT(LOWORD(lParam)), SHORT(HIWORD(lParam)) };
	}

	static constexpr Sys::MouseButton MessageToButton(UINT msg) {
		switch (msg) {
			case WM_LBUTTONDOWN:
			case WM_LBUTTONUP:
				return Sys::MouseButton::Left;
			case WM_MBUTTONDOWN:
			case WM_MBUTTONUP:
				return Sys::MouseButton::Middle;
			case WM_RBUTTONDOWN:
			case WM_RBUTTONUP:
				return Sys::MouseButton::Right;
			default:
				return Sys::MouseButton::Unknown;
		}
	}

	constexpr Sys::MouseButton GetMouseButton(UINT msg) {
		using Sys::MouseButton;
		if (UINT(msg - WM_LBUTTONDOWN) < 3)
			return MouseButton::Left;
		if (UINT(msg - WM_RBUTTONDOWN) < 3)
			return MouseButton::Right;
		if (UINT(msg - WM_MBUTTONDOWN) < 3)
			return MouseButton::Middle;
		return MouseButton::Unknown;
	}

	static constexpr auto EventToMouseButton =
		MakeLookupTable<UINT, Sys::MouseButton, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK>(GetMouseButton);

	static LRESULT CALLBACK WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
		using Sys::Window;
		using Event = Sys::Window::Event;

		auto* window = (Window*)GetWindowLongPtrA(hWnd, 0);
		Event event;
		event.window = window;

		switch (msg) {
			case WM_CREATE: {
				auto& info = *(CREATESTRUCT*)lParam;
				SetWindowLongPtrA(hWnd, 0, (LONG_PTR)info.lpCreateParams);
				return 0;
			}

			case WM_DESTROY: {
				PostQuitMessage(0);
				return 0;
			}

			case WM_ACTIVATE: {
				RECT rect;
				GetWindowRect((HWND)window->handle, &rect);
				POINT center;
				center.x = (rect.left + rect.right) / 2;
				center.y = (rect.bottom + rect.top) / 2;
				if (wParam != WA_INACTIVE) {
					if ((window->flags & (Window::Flags::Active|Window::Flags::FPSMode)) == Window::Flags::FPSMode) {
						ClipCursor(&rect);
						ShowCursor(FALSE);
					}
					window->flags |= Window::Flags::Active;
				} else {
					if ((window->flags & (Window::Flags::Active|Window::Flags::FPSMode)) == (Window::Flags::Active|Window::Flags::FPSMode)) {
						ClipCursor(NULL);
						ShowCursor(TRUE);
					}
					window->flags &= ~Window::Flags::Active;
				}
				SetCursorPos(center.x, center.y);
				return 0;
			}

			case WM_KEYDOWN: {
				g_repeat_key = wParam;
				g_repeat_flag = true;
				return 0;
			}

#ifdef USE_RAW_INPUT
			case WM_INPUT: {
				RAWINPUT raw;
				UINT data_size = sizeof(raw);
				if (::GetRawInputData((HRAWINPUT)lParam, RID_INPUT, &raw, &data_size, sizeof(RAWINPUTHEADER)) != UINT(-1)) {
					if (raw.header.dwType == RIM_TYPEMOUSE) {
						auto& mouse = raw.data.mouse;
						if (!(raw.data.mouse.usFlags & MOUSE_MOVE_ABSOLUTE)) {
							g_mouse_x += mouse.lLastX;
							g_mouse_y += mouse.lLastY;
						}
					}
				}
				return 0;
			}
#endif // USE_RAW_INPUT

#ifdef USE_EVENTS
			case WM_MOUSEMOVE: {
				event.type = Event::Type::MouseMove;
				auto pt = GetPoint(lParam);
				if (window->flags & Window::Flags::FPSMode) {
					if (!(window->flags & Window::Flags::Active))
						return 0;
					RECT rect;
					GetWindowRect((HWND)window->handle, &rect);
					POINT center;
					center.x = (rect.left + rect.right) / 2;
					center.y = (rect.bottom + rect.top) / 2;
					pt.x -= center.x;
					pt.y -= center.y;
					if ((pt.x | pt.y) == 0)
						return 0;
					SetCursorPos(center.x, center.y);
				}
				event.data.mouse_move.pt = pt;
				window->on_event(event);
				return 0;
			}

			case WM_PAINT: {
				PAINTSTRUCT ps;
				BeginPaint(hWnd, &ps);
				event.type = Event::Type::Paint;
				window->on_event(event);
				EndPaint(hWnd, &ps);
				return 0;
			}

			case WM_LBUTTONDOWN:
			case WM_RBUTTONDOWN:
			case WM_MBUTTONDOWN: {
				event.type = Event::Type::MouseDown;
				event.data.mouse_down.pt = GetPoint(lParam);
				event.data.mouse_down.button = GetMouseButton(msg);
				window->on_event(event);
				return 0;
			}

			case WM_LBUTTONUP:
			case WM_RBUTTONUP:
			case WM_MBUTTONUP: {
				event.type = Event::Type::MouseUp;
				event.data.mouse_up.pt = GetPoint(lParam);
				event.data.mouse_up.button = GetMouseButton(msg);
				window->on_event(event);
				return 0;
			}

			case WM_SYSKEYDOWN:
				if (wParam == VK_F4)
					break;
				/* fall-through */
			case WM_KEYDOWN:
			case WM_SYSKEYUP:
			case WM_KEYUP: {
				event.type = (msg == WM_KEYDOWN || msg == WM_SYSKEYDOWN) ? Event::Type::KeyDown : Event::Type::KeyUp;
				event.data.key_down.code = wParam;
				window->on_event(event);
				return 0;
			}
#endif // USE_EVENTS

			case WM_ERASEBKGND:
				return 1;

			default:
				break;
		}

		return DefWindowProcA(hWnd, msg, wParam, lParam);
	}
}

void* CreateSystemWindow(Sys::Window* window);
void* CreateSystemRenderer(Sys::Window* window);

////////////////////////////////////////////////////////////////

NOINLINE Sys::Time Sys::GetTime() {
	LARGE_INTEGER now;
	QueryPerformanceCounter(&now);
	return ((double)now.QuadPart - (double)Win32::g_start_time.QuadPart) * Win32::g_rcp_frequency;
}

////////////////////////////////////////////////////////////////

FORCEINLINE void* Sys::Alloc(size_t size) {
	void* result = VirtualAlloc(NULL, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
	if (!result)
		Sys::Fatal(Error::OutOfMemory);
	return result;
}

FORCEINLINE void Sys::Free(void* alloc) {
	VirtualFree(alloc, 0, MEM_RELEASE);
}

[[noreturn]] FORCEINLINE void Sys::Exit(int code) {
	ExitProcess(code);
}

namespace Win32::FatalError {
	static constexpr char
		Prefix[] = "Error code: ",
		Title[] = "Fatal error"
	;
	char g_buffer[256];
}

[[noreturn]] NOINLINE void Sys::Fatal(int code) {
#ifdef DEV
	DebugBreak();
#endif
	char *p = Win32::FatalError::g_buffer;
	p += CopyStaticString(p, Win32::FatalError::Prefix);
	IntToString(code, p);

	MessageBoxA(0, Win32::FatalError::g_buffer, Win32::FatalError::Title, MB_OK | MB_ICONERROR | MB_APPLMODAL);
	Sys::Exit(code);
}

FORCEINLINE void Sys::Log(const char* text) {
	OutputDebugStringA(text);
}

FORCEINLINE void Sys::DebugLog(const char* text) {
#ifdef DEV
	Sys::Log(text);
#endif
}

FORCEINLINE void Sys::Breakpoint() {
#ifdef DEV
	DebugBreak();
#endif
}

////////////////////////////////////////////////////////////////
// Threading ///////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

namespace Win32 {
	DWORD WINAPI StartThread(void* data) {
		auto thread = (Sys::Thread*)data;
		thread->work(thread->data);
		return 0;
	}

	static_assert(sizeof(HANDLE) <= sizeof(void*), "Thread handle too small");
}

FORCEINLINE u32 Sys::GetNumCPUThreads() {
	SYSTEM_INFO info;
	GetSystemInfo(&info);
	return info.dwNumberOfProcessors;
}

NOINLINE void Sys::SpawnThread(Thread& thread) {
	HANDLE handle = CreateThread(NULL, 0, &Win32::StartThread, &thread, 0, NULL);
	assert(handle != INVALID_HANDLE_VALUE);
	thread.handle = (void*)handle;
}

NOINLINE void Sys::JoinThread(Thread& thread) {
	assert((HANDLE)thread.handle != INVALID_HANDLE_VALUE);
	DWORD result = WaitForSingleObject((HANDLE)thread.handle, INFINITE);
	assert(result == WAIT_OBJECT_0);
	CloseHandle((HANDLE)thread.handle);
#ifdef DEV
	thread.handle = (void*)INVALID_HANDLE_VALUE;
#endif
}

FORCEINLINE bool Sys::IsThreadReady(const Thread& thread) {
	assert((HANDLE)thread.handle != INVALID_HANDLE_VALUE);
	DWORD result = WaitForSingleObject((HANDLE)thread.handle, 0);
	assert(result == WAIT_OBJECT_0 || result == WAIT_TIMEOUT);
	return result == WAIT_OBJECT_0;
}

////////////////////////////////////////////////////////////////
// File handling ///////////////////////////////////////////////
////////////////////////////////////////////////////////////////

namespace Win32 {
	static constexpr DWORD FileOpenParams[2][3] = {
		{GENERIC_READ, FILE_SHARE_READ, OPEN_EXISTING},
		{GENERIC_WRITE, 0, CREATE_ALWAYS},
	};
}

FORCEINLINE Sys::File::Handle Sys::OpenFile(const char* path, File::Mode mode) {
	const DWORD* arg = Win32::FileOpenParams[mode == File::Write];
	HANDLE handle = ::CreateFileA(path, arg[0], arg[1], NULL, arg[2], FILE_ATTRIBUTE_NORMAL, NULL);
	return {(void*)handle};
}

FORCEINLINE bool Sys::IsOpen(File::Handle file) {
	return (HANDLE)file.data != INVALID_HANDLE_VALUE;
}

FORCEINLINE void Sys::CloseFile(File::Handle& file) {
	if ((HANDLE)file.data != INVALID_HANDLE_VALUE) {
		::CloseHandle((HANDLE)file.data);
		file.data = (void*)INVALID_HANDLE_VALUE;
	}
}

FORCEINLINE bool Sys::ReadFromFile(File::Handle file, void* buffer, u32 size, u32* read) {
	DWORD bytes_read;
	BOOL result = ::ReadFile((HANDLE)file.data, buffer, size, &bytes_read, NULL);
	if (read)
		*read = bytes_read;
	return result != FALSE;
}

FORCEINLINE bool Sys::WriteToFile(File::Handle file, const void* buffer, u32 size, u32* written) {
	DWORD bytes_written;
	BOOL result = ::WriteFile((HANDLE)file.data, buffer, size, &bytes_written, NULL);
	if (written)
		*written = bytes_written;
	return result != FALSE;
}

FORCEINLINE bool Sys::CreateFolder(const char* name) {
	return ::CreateDirectoryA(name, NULL) || ::GetLastError() == ERROR_ALREADY_EXISTS;
}

////////////////////////////////////////////////////////////////

namespace Win32 {
	using BitmapHeaderType = BITMAPV4HEADER;

	static constexpr BitmapHeaderType BaseBitmapHeader = {
		sizeof(BitmapHeaderType),	// DWORD        bV4Size;
		0,							// LONG         bV4Width;
		0,							// LONG         bV4Height;
		1,							// WORD         bV4Planes;
		32,							// WORD         bV4BitCount;
		BI_BITFIELDS,				// DWORD        bV4V4Compression;
		0,							// DWORD        bV4SizeImage;
		0,							// LONG         bV4XPelsPerMeter;
		0,							// LONG         bV4YPelsPerMeter;
		0,							// DWORD        bV4ClrUsed;
		0,							// DWORD        bV4ClrImportant;
		0x00FF0000,					// DWORD        bV4RedMask;
		0x0000FF00,					// DWORD        bV4GreenMask;
		0x000000FF,					// DWORD        bV4BlueMask;
		0xFF000000,					// DWORD        bV4AlphaMask;
		0,							// DWORD        bV4CSType;
		{},							// CIEXYZTRIPLE bV4Endpoints;
		0,							// DWORD        bV4GammaRed;
		0,							// DWORD        bV4GammaGreen;
		0,							// DWORD        bV4GammaBlue;
	};

	static constexpr MAT2 IdentityMAT2 = {{0, 1}, {0, 0}, {0, 0}, {0, 1}};
}

FORCEINLINE void Sys::SetWindowIcon(Window* window, const u32* pixels, u16 size) {
	Win32::BitmapHeaderType bi;
	MemCopy(&bi, &Win32::BaseBitmapHeader);

	bi.bV4Width = size;
	bi.bV4Height = size;

	HDC hdc = GetDC(NULL);

	void *lpBits;
	HBITMAP hBitmap = CreateDIBSection(hdc, (BITMAPINFO*)&bi, DIB_RGB_COLORS, &lpBits, NULL, 0);

	MemCopy(lpBits, pixels, size * size * sizeof(u32));

	// Create an empty mask bitmap.
	HBITMAP hMonoBitmap = CreateBitmap(size, size, 1, 1, NULL);

	ICONINFO icon_info;
	icon_info.fIcon = TRUE;
	icon_info.xHotspot = 0;
	icon_info.yHotspot = 0;
	icon_info.hbmMask = hMonoBitmap;
	icon_info.hbmColor = hBitmap;

	HICON hIcon = CreateIconIndirect(&icon_info);
	ReleaseDC(NULL, hdc);
	DeleteObject(hBitmap);

	hIcon = (HICON) SendMessageA((HWND)window->handle, WM_SETICON, ICON_SMALL, (LPARAM)hIcon);
	if (hIcon)
		DestroyIcon(hIcon);
}

FORCEINLINE void Sys::RasterizeFont(const char* name, int font_size, u32 flags, u32* pixels, u16 width, u16 height, u16 padding, RectPacker& packer, Font::Glyph* glyphs) {
	using namespace Font;

	padding += 4;

	HFONT font = CreateFontA(
		-font_size,									// height
		0,											// width
		0,											// escapement
		0,											// orientation
		flags & Flags::Bold ? FW_BOLD : FW_NORMAL,	// weight
		flags & Flags::Italic ? TRUE : FALSE,		// italic
		FALSE,										// underline
		FALSE,										// strike-out
		ANSI_CHARSET,								// charset
		OUT_OUTLINE_PRECIS,							// output precision
		CLIP_DEFAULT_PRECIS,						// clipping precision
		ANTIALIASED_QUALITY,						// output quality
		VARIABLE_PITCH,								// pitch and family
		name										// font name
	);

	assert(font);
	if (!font)
		Sys::Fatal(Error::Font);

	Win32::BitmapHeaderType bi;
	MemCopy(&bi, &Win32::BaseBitmapHeader);

	bi.bV4Width = width;
	bi.bV4Height = height;

	HDC hdc = GetDC(NULL);

	void *lpBits;
	HBITMAP hBitmap = CreateDIBSection(hdc, (BITMAPINFO*)&bi, DIB_RGB_COLORS, &lpBits, NULL, 0);

	HDC hMemDC = CreateCompatibleDC(hdc);
	ReleaseDC(NULL, hdc);

	HBITMAP hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);
	HFONT hOldFont = (HFONT)SelectObject(hMemDC, font);

	const u32 MAX_GLYPH_SIZE = 64 * 64;
	u8 buffer[MAX_GLYPH_SIZE];

	MemSet(glyphs, 0, Font::Glyph::Count);

	for (u16 c = Font::Glyph::Begin; c < Font::Glyph::End; ++c) {
		GLYPHMETRICS metrics;
		DWORD required = GetGlyphOutlineA(hMemDC, c, GGO_GRAY8_BITMAP, &metrics, 0, NULL, &Win32::IdentityMAT2);
		assert(required <= size(buffer));
		if (required > size(buffer))
			continue;
		if (GDI_ERROR == GetGlyphOutlineA(hMemDC, c, GGO_GRAY8_BITMAP, &metrics, required, buffer, &Win32::IdentityMAT2))
			Sys::Fatal(Error::Font);

		auto& g = glyphs[c - Font::Glyph::Begin];
		if (required > 0) {
			auto tile = packer.Add(metrics.gmBlackBoxX + padding * 2, metrics.gmBlackBoxY + padding * 2);
			assert(tile != packer.Full);
			auto& rect = packer.GetTile(tile);

			u32 src_pitch = (metrics.gmBlackBoxX + 3) & ~3;
			u8* src_pixels = buffer + (metrics.gmBlackBoxY - 1) * src_pitch; // flip vertically
			u32* dst_pixels = pixels + (rect.min[1] + padding) * width + rect.min[0] + padding;
			for (u16 y = 0; y < metrics.gmBlackBoxY; ++y, src_pixels -= src_pitch, dst_pixels += width) {
				for (u16 x = 0; x < metrics.gmBlackBoxX; ++x) {
					u8 src = src_pixels[x];
					if (src > 63)
						src = 63;						// [0-64] to [0-63]
					src = (src * 65u) >> 4;				// [0-63] to [0-255]
					dst_pixels[x] = src * 0x01010101u;	// intensity to BGRA premultiplied
				}
			}

			static_assert(sizeof(g.box_min) == sizeof(u32));
			static_assert(sizeof(rect.min) == sizeof(u32));

			*(u32*)g.box_min = *(u32*)rect.min;
			g.box_size[0] = (u8)rect.GetWidth();
			g.box_size[1] = (u8)rect.GetHeight();
			g.anchor[0] = i8(metrics.gmptGlyphOrigin.x - padding);
			g.anchor[1] = i8(metrics.gmptGlyphOrigin.y - metrics.gmBlackBoxY - padding);
		}

		g.advance = (u8)metrics.gmCellIncX;
	}

	::SelectObject(hMemDC, hOldFont);
	::SelectObject(hMemDC, hOldBitmap);
	DeleteDC(hMemDC);
	DeleteObject(font);
}

////////////////////////////////////////////////////////////////

[[noreturn]] void WINAPI WinMainCRTStartup() {
	LARGE_INTEGER frequency;
	QueryPerformanceFrequency(&frequency);
	QueryPerformanceCounter(&Win32::g_start_time);
	Win32::g_rcp_frequency = 1. / frequency.QuadPart;
	Seed(Win32::g_start_time.LowPart);
	Win32::g_waitable_timer = ::CreateWaitableTimerA(NULL, FALSE, NULL);
	int code = demo_main();
	ExitProcess(code);
}

[[noreturn]] void mainCRTStartup() {
	WinMainCRTStartup();
}

namespace Win32 {
	const DWORD WindowStyle		= WS_POPUP | WS_VISIBLE;
	const DWORD WindowStyleEx	= 0;
	const DWORD ClassStyle		= CS_HREDRAW | CS_VREDRAW | CS_OWNDC;

	static constexpr WNDCLASSEXA BaseWindowClass = {
		sizeof(WNDCLASSEXA),	// UINT        cbSize;
		ClassStyle,				// UINT        style;
		&WndProc,				// WNDPROC     lpfnWndProc;
		0,						// int         cbClsExtra;
		sizeof(Sys::Window*),	// int         cbWndExtra;
		NULL,					// HINSTANCE   hInstance;
		NULL,					// HICON       hIcon;
		NULL,					// HCURSOR     hCursor;
		NULL,					// HBRUSH      hbrBackground;
		NULL,					// LPCSTR      lpszMenuName;
		NULL,					// LPCSTR      lpszClassName;
		NULL,					// HICON       hIconSm;
	};

	static constexpr RAWINPUTDEVICE BaseRIMouse = {
		1,		// USHORT usUsagePage; // Toplevel collection UsagePage
		2,		// USHORT usUsage;     // Toplevel collection Usage
		0,		// DWORD dwFlags;
		NULL,	// HWND hwndTarget;    // Target hwnd. NULL = follows keyboard focus
	};
}

FORCEINLINE void* CreateSystemWindow(Sys::Window* window) {
	const CHAR* ClassName		= window->title;

	HINSTANCE hInstance = GetModuleHandleA(NULL);

	WNDCLASSEXA wndClassEx;
	MemCopy(&wndClassEx, &Win32::BaseWindowClass);
	wndClassEx.hInstance        = hInstance;
	wndClassEx.hCursor          = LoadCursor(NULL, IDC_ARROW);
	wndClassEx.hbrBackground    = (HBRUSH)GetStockObject(BLACK_BRUSH);
	//wndClassEx.hbrBackground	= (HBRUSH)(COLOR_WINDOW + 1);
	wndClassEx.lpszClassName    = ClassName;

	RegisterClassExA(&wndClassEx);

	// get info for primary monitor
	const POINT ptRef = { 0, 0 };
	HMONITOR hmon = MonitorFromPoint(ptRef, MONITOR_DEFAULTTOPRIMARY);
	MONITORINFOEXA monitorinfo;
	monitorinfo.cbSize = sizeof(monitorinfo);
	GetMonitorInfoA(hmon, &monitorinfo);

	RECT rect = monitorinfo.rcMonitor;

	window->width  = i16(rect.right - rect.left);
	window->height = i16(rect.bottom - rect.top);

	auto handle = CreateWindowExA(Win32::WindowStyleEx, ClassName, window->title, Win32::WindowStyle,
		rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top,
		NULL, NULL, hInstance, window);

	DEVMODEA dev;
	dev.dmSize = sizeof(dev);
	dev.dmDriverExtra = 0;
	if (EnumDisplaySettingsA(monitorinfo.szDevice, ENUM_CURRENT_SETTINGS, &dev) && (dev.dmFields & DM_DISPLAYFREQUENCY))
		window->refresh = (i16)dev.dmDisplayFrequency;

#ifdef USE_RAW_INPUT
	RAWINPUTDEVICE rid;
	MemCopy(&rid, &Win32::BaseRIMouse);
	rid.hwndTarget = (HWND)window->handle;

	if (RegisterRawInputDevices(&rid, 1, sizeof(rid))) {
		Sys::DebugLog("Raw input initialized.\n");
	} else {
		Sys::DebugLog("ERROR: RegisterRawInputDevices failed.\n");
	}
#endif // USE_RAW_INPUT

	return handle;
}

FORCEINLINE void Sys::InitWindow(Window* window, Window::Event::Handler handler, const char* name) {
	window->on_event		= handler;
	window->title			= name;
	window->refresh			= 60;
	window->handle			= CreateSystemWindow(window);
	window->render_context	= CreateSystemRenderer(window);
}

FORCEINLINE void Sys::RedrawWindow(Window* window) {
	HWND hWnd = (HWND)window->handle;
	RedrawWindow(hWnd, NULL, NULL, RDW_INVALIDATE | RDW_NOERASE | RDW_INTERNALPAINT | RDW_UPDATENOW);
}

void Sys::SetFPSMode(Window* window, bool enabled) {
	bool already_enabled = (window->flags & Window::Flags::FPSMode) != 0;
	if (enabled == already_enabled)
		return;

	if (enabled) {
		window->flags |= Window::Flags::FPSMode;
		RECT rect;
		GetWindowRect((HWND)window->handle, &rect);
		ClipCursor(&rect);
		POINT center;
		center.x = (rect.left + rect.right) / 2;
		center.y = (rect.bottom + rect.top) / 2;
		SetCursorPos(center.x, center.y);
	} else {
		window->flags &= ~Window::Flags::FPSMode;
		ClipCursor(NULL);
	}
	ShowCursor(!enabled);
}

FORCEINLINE bool Sys::PumpMessages(int* error) {
	MSG msg;

	while (PeekMessageA(&msg, NULL, 0, 0, PM_REMOVE)) {
		if (msg.message == WM_QUIT && msg.hwnd == 0) {
			if (error)
				*error = msg.wParam;
			return false;
		}
		TranslateMessage(&msg);
		DispatchMessageA(&msg);
	}

	return true;
}

FORCEINLINE void Sys::Sleep(float seconds) {
	::Sleep(DWORD(seconds * 1000.f));
}

// For use on main thread only
FORCEINLINE void Sys::PreciseSleep(float seconds) {
	LARGE_INTEGER due_time;
	due_time.QuadPart = LONGLONG(-seconds * 1e7f); // 100 nanosecond intervals
	if (::SetWaitableTimer(Win32::g_waitable_timer, &due_time, 0, NULL, NULL, FALSE)) {
		DWORD result = ::WaitForSingleObject(Win32::g_waitable_timer, INFINITE);
		assert(result == WAIT_OBJECT_0);
	} else {
		assert(false);
		//Sys::Sleep(seconds);
	}
}

FORCEINLINE int Sys::RunApplication() {
	MSG msg;
	BOOL bRet;

	while ((bRet = GetMessageA(&msg, NULL, 0, 0)) != 0) {
		if (bRet == -1) {
			return 666;
		} else {
			TranslateMessage(&msg);
			DispatchMessageA(&msg);
		}
	}

	return (int)msg.wParam;
}

FORCEINLINE void Sys::UpdateKeyboardState() {
	using namespace Win32;

	BYTE current_state[256];
	if (g_window.flags & Window::Flags::Active)
		::GetKeyboardState(current_state);
	else
		MemSet(current_state, 0, 256);

	if (!g_repeat_flag)
		g_repeat_key = 0;

	for (u16 i = 0; i < 256; ++i) {
		u8 down = current_state[i] >> 7;
		g_key_state[i] = ((g_key_state[i] & 1) << 1) | down;
		// disable auto-repeat when multiple keys are pressed
		if (down && i != g_repeat_key && g_repeat_key)
			g_repeat_key = 0;
	}

	g_repeat_flag = false;
}

FORCEINLINE bool Sys::IsKeyRepeating(u8 key) {
	return Win32::g_repeat_key == key;
}

FORCEINLINE void Sys::UpdateMouseState(vec2& pt, float dt) {
	using namespace Win32;

#ifdef USE_RAW_INPUT
	if (g_window.flags & Window::Flags::FPSMode) {
		float scale = 1.f;

		INT win_speed, win_accel[3];
		// TODO: cache SPI values?
		if (::SystemParametersInfoA(SPI_GETMOUSESPEED, 0, &win_speed, FALSE) &&
			::SystemParametersInfoA(SPI_GETMOUSE, 0, win_accel, FALSE))
		{
			const float
				AccelMin = 0.5f, // min scale factor
				AccelMax = 2.5f, // max scale factor
				MaxSpeed = 5.0f // speed, in screens per second, at which we use the max scale factor
			;

			scale = win_speed / 10.f;
			int max_dist = max(abs(g_mouse_x), abs(g_mouse_y));
			float speed = max_dist * scale / dt / g_window.width;
			if (win_accel[2])
				scale *= mix(AccelMin, AccelMax, min(1.f, speed / MaxSpeed));
		}

		pt.x = g_mouse_x * scale;
		pt.y = g_mouse_y * scale;
		g_mouse_x = 0;
		g_mouse_y = 0;
	} else {
		POINT p;
		::GetCursorPos(&p);

		pt.x = (float)p.x;
		pt.y = (float)p.y;
	}
#else
	POINT p;
	::GetCursorPos(&p);

	pt.x = p.x;
	pt.y = p.y;

	if (g_window.flags & Window::Flags::FPSMode) {
		RECT rect;
		::GetWindowRect((HWND)g_window.handle, &rect);
		POINT center;
		center.x = (rect.left + rect.right) / 2;
		center.y = (rect.bottom + rect.top) / 2;
		if (g_window.flags & Window::Flags::Active) {
			::SetCursorPos(center.x, center.y);
			pt.x -= center.x;
			pt.y -= center.y;
		} else {
			MemSet(&pt);
		}
	}
#endif // USE_RAW_INPUT
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

namespace Win32 {
	const char* (WINAPI *wglGetExtensionsStringARB)(HDC hdc);
	HGLRC (WINAPI* wglCreateContextAttribsARB)(HDC hDC, HGLRC hShareContext, const int *attribList);

	template <typename Function>
	FORCEINLINE void InitGLFunction(Function*& function, const char* name) {
		function = (Function*)wglGetProcAddress(name);
	}

	void InitWGLExtensions(HDC dc) {
		InitGLFunction(wglGetExtensionsStringARB, "wglGetExtensionsStringARB");
		if (!wglGetExtensionsStringARB)
			return;

		const char* str = wglGetExtensionsStringARB(dc);
		while (*str) {
			u32 hash = 0;
			while (*str && *str != ' ')
				hash = HashAppend(hash, *str++);
			if (*str == ' ')
				++str;
			if (hash == Hash("WGL_ARB_create_context")) {
				InitGLFunction(wglCreateContextAttribsARB, "wglCreateContextAttribsARB");
			}
		}
	}

	HWND CreateDummyWindow() {
		constexpr auto ClassName = "Static";
		return CreateWindow(ClassName, ClassName, WS_DISABLED, 0, 0, 0, 0, NULL, NULL, NULL, NULL);
	}

	FORCEINLINE HGLRC CreateGLContext(HDC dc) {
		static const PIXELFORMATDESCRIPTOR pfd = {
			sizeof(pfd), 1,
			PFD_SUPPORT_OPENGL|PFD_DRAW_TO_WINDOW|PFD_DOUBLEBUFFER, PFD_TYPE_RGBA,
			32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32
		};

		int pixelFormat = ChoosePixelFormat(dc, &pfd);
		if (pixelFormat == 0 || SetPixelFormat(dc, pixelFormat, &pfd) != TRUE)
			Sys::Fatal(Error::BadPixelFormat);

		return wglCreateContext(dc);
	}
}

////////////////////////////////////////////////////////////////

#define WGL_CONTEXT_DEBUG_BIT_ARB      0x00000001
#define WGL_CONTEXT_MAJOR_VERSION_ARB  0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB  0x2092
#define WGL_CONTEXT_FLAGS_ARB          0x2094

/* WGL_ARB_create_context_profile */
#define WGL_CONTEXT_PROFILE_MASK_ARB   0x9126
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB 0x00000001
#define WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB 0x00000002
#define ERROR_INVALID_PROFILE_ARB      0x2096

////////////////////////////////////////////////////////////////

static FORCEINLINE HGLRC CreateGLContext(Sys::Window* window) {
	HWND hwnd = (HWND)window->handle;
	HDC dc = GetDC(hwnd);

#ifdef ENABLE_RENDERDOC
	HWND dummy_window = Win32::CreateDummyWindow();
	HDC dummy_dc = GetDC(dummy_window);
	HGLRC dummy_rc = Win32::CreateGLContext(dummy_dc);
	wglMakeCurrent(dummy_dc, dummy_rc);
	Win32::InitWGLExtensions(dummy_dc);

	if (dummy_rc) {
		wglMakeCurrent(NULL, NULL);
		wglDeleteContext(dummy_rc);
	}
	if (dummy_window)
		DestroyWindow(dummy_window);

	static const PIXELFORMATDESCRIPTOR pfd = {
		sizeof(pfd), 1,
		PFD_SUPPORT_OPENGL|PFD_DRAW_TO_WINDOW|PFD_DOUBLEBUFFER, PFD_TYPE_RGBA,
		32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32
	};
	int pixelFormat = ChoosePixelFormat(dc, &pfd);
	if (pixelFormat == 0 || SetPixelFormat(dc, pixelFormat, &pfd) != TRUE)
		Sys::Fatal(Error::BadPixelFormat);

	HGLRC rc = NULL;
	if (Win32::wglCreateContextAttribsARB) {
		const int MaxNumAttribs = 16;
		int attribs[MaxNumAttribs * 2];
		int num_attribs = 0;

		auto set = [&] (int key, int value) {
			attribs[num_attribs++] = key;
			attribs[num_attribs++] = value;
		};
		
		set(WGL_CONTEXT_MAJOR_VERSION_ARB,	3);
		set(WGL_CONTEXT_MINOR_VERSION_ARB,	3);
		set(WGL_CONTEXT_FLAGS_ARB,			WGL_CONTEXT_DEBUG_BIT_ARB);
		set(WGL_CONTEXT_PROFILE_MASK_ARB,	WGL_CONTEXT_CORE_PROFILE_BIT_ARB);
		attribs[num_attribs++] = 0;

		rc = Win32::wglCreateContextAttribsARB(dc, NULL, attribs);
	} else {
		rc = Win32::CreateGLContext(dc);
	}

	if (!wglMakeCurrent(dc, rc))
		Sys::Fatal(Error::RenderContext);

	return rc;
#else
	HGLRC rc = Win32::CreateGLContext(dc);
	if (!wglMakeCurrent(dc, rc))
		Sys::Fatal(Error::RenderContext);

	return rc;
#endif
}
