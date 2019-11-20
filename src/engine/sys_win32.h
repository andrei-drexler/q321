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
	LARGE_INTEGER g_startTime;
	double g_rcpFrequency;

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
				if (wParam != WA_INACTIVE) {
					if ((window->flags & (Window::Flags::Active|Window::Flags::FPSMode)) == Window::Flags::FPSMode) {
						RECT rect;
						GetWindowRect((HWND)window->handle, &rect);
						POINT center;
						center.x = (rect.left + rect.right) / 2;
						center.y = (rect.bottom + rect.top) / 2;
						ClipCursor(&rect);
						SetCursorPos(center.x, center.y);
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
				return 0;
			}

			case WM_PAINT: {
				PAINTSTRUCT ps;
				BeginPaint(hWnd, &ps);
				event.type = Event::Type::Paint;
				window->on_event(event);
				SwapBuffers(ps.hdc);
				// trade off some performance for lower input lag
				Gfx::Sync();
				EndPaint(hWnd, &ps);
				return 0;
			}

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

FORCEINLINE Sys::Time Sys::GetTime() {
	LARGE_INTEGER now;
	QueryPerformanceCounter(&now);
	return ((double)now.QuadPart - (double)Win32::g_startTime.QuadPart) * Win32::g_rcpFrequency;
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

[[noreturn]] FORCEINLINE void Sys::Fatal(int code) {
	const char* message = "Fatal error: insufficient cowbell.";
#ifdef DEV
	DebugBreak();
#endif
	MessageBoxA(0, message, "Error", MB_OK | MB_ICONERROR | MB_APPLMODAL);
	ExitProcess(code);
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

FORCEINLINE void Sys::SpawnThread(Thread& thread) {
	HANDLE handle = CreateThread(NULL, 0, &Win32::StartThread, &thread, 0, NULL);
	assert(handle != INVALID_HANDLE_VALUE);
	thread.handle = (void*)handle;
}

FORCEINLINE void Sys::JoinThread(Thread& thread) {
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

NOINLINE Sys::File::Handle Sys::OpenFile(const char* path, File::Mode mode) {
	DWORD access, share, creation;
	if (mode == File::Mode::Write) {
		access = GENERIC_WRITE;
		share = 0;
		creation = CREATE_ALWAYS;
	} else {
		access = GENERIC_READ;
		share = FILE_SHARE_READ;
		creation = OPEN_EXISTING;
	}

	HANDLE handle = CreateFileA(path, access, share, NULL, creation, FILE_ATTRIBUTE_NORMAL, NULL);
	return {(void*)handle};
}

FORCEINLINE bool Sys::IsOpen(File::Handle file) {
	return (HANDLE)file.data != INVALID_HANDLE_VALUE;
}

FORCEINLINE void Sys::CloseFile(File::Handle& file) {
	if ((HANDLE)file.data != INVALID_HANDLE_VALUE) {
		CloseHandle((HANDLE)file.data);
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

////////////////////////////////////////////////////////////////

FORCEINLINE void Sys::RasterizeFont(const char* name, int font_size, u32 flags, u32* pixels, u16 width, u16 height, RectPacker& packer, Font::Glyph* glyphs) {
	using namespace Font;

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

	BITMAPV5HEADER bi;
	ZeroMemory(&bi, sizeof(BITMAPV5HEADER));

	bi.bV5Size = sizeof(BITMAPV5HEADER);
	bi.bV5Width = width;
	bi.bV5Height = height;
	bi.bV5Planes = 1;
	bi.bV5BitCount = 32;
	bi.bV5Compression = BI_BITFIELDS;
	bi.bV5RedMask = 0x00FF0000;
	bi.bV5GreenMask = 0x0000FF00;
	bi.bV5BlueMask = 0x000000FF;
	bi.bV5AlphaMask = 0xFF000000;

	HDC hdc = GetDC(NULL);

	void *lpBits;
	HBITMAP hBitmap = CreateDIBSection(hdc, (BITMAPINFO*)&bi, DIB_RGB_COLORS, &lpBits, NULL, 0);

	HDC hMemDC = CreateCompatibleDC(hdc);
	ReleaseDC(NULL, hdc);

	HBITMAP hOldBitmap = (HBITMAP)SelectObject(hMemDC, hBitmap);
	HFONT hOldFont = (HFONT)SelectObject(hMemDC, font);

	const u32 MAX_GLYPH_SIZE = 64 * 64;
	u8 buffer[MAX_GLYPH_SIZE];

	MemSet(glyphs, 0, sizeof(Font::Glyph) * Font::Glyph::Count);

	for (u16 c = Font::Glyph::Begin; c < Font::Glyph::End; ++c) {
		GLYPHMETRICS metrics;
		constexpr MAT2 identity = { 0, 1, 0, 0, 0, 0, 0, 1 };
		DWORD required = GetGlyphOutlineA(hMemDC, c, GGO_GRAY8_BITMAP, &metrics, 0, NULL, &identity);
		assert(required <= size(buffer));
		if (required > size(buffer))
			continue;
		if (GDI_ERROR == GetGlyphOutlineA(hMemDC, c, GGO_GRAY8_BITMAP, &metrics, required, buffer, &identity))
			Sys::Fatal(Error::Font);

		auto& g = glyphs[c - Font::Glyph::Begin];
		if (required > 0) {
			const u16 Padding = 4;

			auto tile = packer.Add(metrics.gmBlackBoxX + Padding * 2, metrics.gmBlackBoxY + Padding * 2);
			assert(tile != packer.Full);
			auto& rect = packer.GetTile(tile);

			u32 src_pitch = (metrics.gmBlackBoxX + 3) & ~3;
			u8* src_pixels = buffer + (metrics.gmBlackBoxY - 1) * src_pitch; // flip vertically
			u32* dst_pixels = pixels + (rect.min[1] + Padding) * width + rect.min[0] + Padding;
			for (u16 y = 0; y < metrics.gmBlackBoxY; ++y, src_pixels -= src_pitch, dst_pixels += width) {
				for (u16 x = 0; x < metrics.gmBlackBoxX; ++x) {
					u8 src = src_pixels[x];
					if (src > 63)
						src = 63;						// [0-64] to [0-63]
					src = (src * 65u) >> 4;				// [0-63] to [0-255]
					dst_pixels[x] = src * 0x01010101u;	// intensity to BGRA premultiplied
				}
			}

			g.box_min[0] = rect.min[0] + Padding;
			g.box_min[1] = rect.min[1] + Padding;
			g.box_size[0] = metrics.gmBlackBoxX;
			g.box_size[1] = metrics.gmBlackBoxY;
			g.anchor[0] = metrics.gmptGlyphOrigin.x;
			g.anchor[1] = metrics.gmptGlyphOrigin.y - metrics.gmBlackBoxY;
		}

		g.advance = metrics.gmCellIncX;
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
	QueryPerformanceCounter(&Win32::g_startTime);
	Win32::g_rcpFrequency = 1. / frequency.QuadPart;
	int code = demo_main();
	ExitProcess(code);
}

FORCEINLINE void* CreateSystemWindow(Sys::Window* window) {
	const CHAR* ClassName		= window->title;
	const DWORD WindowStyle		= WS_POPUP | WS_VISIBLE;
	const DWORD WindowStyleEx	= 0;

	HINSTANCE hInstance = GetModuleHandleA(NULL);

	WNDCLASSEXA wndClassEx;
	ZeroMemory(&wndClassEx, sizeof(wndClassEx));
	wndClassEx.cbSize           = sizeof(wndClassEx);
	wndClassEx.style            = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
	wndClassEx.lpfnWndProc      = &Win32::WndProc;
	wndClassEx.cbWndExtra       = sizeof(window);
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

	window->width = rect.right - rect.left;
	window->height = rect.bottom - rect.top;

	auto handle = CreateWindowExA(WindowStyleEx, ClassName, window->title, WindowStyle,
		rect.left, rect.top, rect.right-rect.left, rect.bottom-rect.top,
		NULL, NULL, hInstance, window);

	DEVMODEA dev;
	dev.dmSize = sizeof(dev);
	dev.dmDriverExtra = 0;
	if (EnumDisplaySettingsA(monitorinfo.szDevice, ENUM_CURRENT_SETTINGS, &dev) && (dev.dmFields & DM_DISPLAYFREQUENCY))
		window->refresh = dev.dmDisplayFrequency;

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

FORCEINLINE void Sys::Sleep(float duration) {
	::Sleep(DWORD(duration * 1000.f));
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

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

namespace Win32 {
	const char* (WINAPI *wglGetExtensionsStringARB)(HDC hdc);
	HGLRC (WINAPI* wglCreateContextAttribsARB)(HDC hDC, HGLRC hShareContext, const int *attribList);

	void InitWGLExtensions(HDC dc) {
		*(void**)&wglGetExtensionsStringARB = wglGetProcAddress("wglGetExtensionsStringARB");
		if (!wglGetExtensionsStringARB)
			return;

		const char* str = wglGetExtensionsStringARB(dc);
		while (*str) {
			u32 hash = 0;
			const char* ext = str;
			while (*str && *str != ' ')
				hash = HashAppend(hash, *str++);
			if (*str == ' ')
				++str;
			if (hash == Hash("WGL_ARB_create_context")) {
				*(void**)&wglCreateContextAttribsARB = wglGetProcAddress("wglCreateContextAttribsARB");
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

#ifdef DEV
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
		set(WGL_CONTEXT_PROFILE_MASK_ARB,	WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB);
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
