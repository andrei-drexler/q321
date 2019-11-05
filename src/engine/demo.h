#pragma once

#ifndef NDEBUG
	#define DEV
#endif

#ifndef assert
	#ifndef NDEBUG
		#define assert(condition)											\
			 do {															\
				if (!(condition)) {											\
					Sys::Log("\nAssertion failed:\n\n" #condition "\n\n");	\
					Sys::Breakpoint();										\
				}															\
			 } while (0)
	#else
		#define assert(condition) ((void)(condition))
	#endif // ndef NDEBUG
#endif

#include "common.h"
#include "math.h"
#include "aabb.h"
using namespace Math::CRT;

//FORCEINLINE void* __cdecl operator new(size_t bytes, void* ptr) noexcept { return ptr; }

////////////////////////////////////////////////////////////////

template <typename T, size_t Size>
constexpr FORCEINLINE size_t size(T (&)[Size]) { return Size; }

////////////////////////////////////////////////////////////////

template <typename T>
class array_view {
public:
	constexpr array_view() = default;
	constexpr array_view(const array_view&) = default;
	constexpr array_view(T* begin, T* end) : m_begin(begin), m_end(end) { }
	constexpr array_view(T* begin, size_t size) : m_begin(begin), m_end(begin + size) { }
	constexpr array_view(const T& value) : m_begin(&value), m_end(&value + 1) { }
	
	template <int Size>
	constexpr array_view(T (&arr)[Size]) : m_begin(&arr[0]), m_end(&arr[0] + Size) { }

	template <typename Collection>
	constexpr array_view(Collection&& collection, decltype(data(collection))* _has_data=nullptr, decltype(size(collection))* _has_size=nullptr) :
		array_view(data(collection), size(collection)) { }

	using iterator = T*;
	using const_iterator = const T*;

	constexpr iterator				begin() { return m_begin; }
	constexpr iterator				end() { return m_end; }
	constexpr const_iterator		begin() const { return m_begin; }
	constexpr const_iterator		end() const { return m_end; }

	constexpr bool					empty() const { return m_begin == m_end; }
	constexpr size_t				size() const { return m_end - m_begin; }
	constexpr T*					data() { return m_begin; }
	constexpr const T*				data() const { return m_begin; }
	constexpr T&					operator[](size_t i) { return m_begin[i]; }
	constexpr const T&				operator[](size_t i) const { return m_begin[i]; }

	constexpr T&					front() { return *m_begin; }
	constexpr const T&				front() const { return *m_begin; }

	constexpr T&					back() { return *(m_end-1); }
	constexpr const T&				back() const { return *(m_end-1); }

	void							remove_prefix(size_t count) { m_begin += count; }
	void							remove_suffix(size_t count) { m_end -= count; }

	operator array_view<const T>() const { return {m_begin, m_end}; }
	
private:
	T*								m_begin{nullptr};
	T*								m_end{nullptr};
};

////////////////////////////////////////////////////////////////

struct IRect {
	int x, y, w, h;
};

////////////////////////////////////////////////////////////////

#include "sys.h"
#include "gfx.h"

#include "sys_win32.h"
#include "gfx_opengl.h"

