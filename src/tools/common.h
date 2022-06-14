#pragma once

#define _CRT_SECURE_NO_WARNINGS 1

#include <cstdio>
#include <cstdint>
#include <cstdarg>
#include <cassert>
#include <climits>
#include <cctype>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <charconv>
#include <algorithm>

using namespace std::string_view_literals;
using std::string_view;

////////////////////////////////////////////////////////////////

#include "../engine/core.h"
#include "../engine/math.h"
#include "../engine/aabb.h"

#include "parse.h"
#include "print_array.h"

////////////////////////////////////////////////////////////////

#ifdef _WIN32
	#define WIN32_LEAN_AND_MEAN
	#define WIN32_EXTRA_LEAN
	#define NOMINMAX
	#define _WIN32_WINNT 0x0603
	#include <Windows.h>

	void DebugPrint(const char* format, ...) {
		char buf[8192];

		va_list args;
		va_start(args, format);
		vsnprintf(buf, std::size(buf), format, args);
		va_end(args);
	
		buf[std::size(buf)-1] = 0;
		
		OutputDebugStringA(buf);
	}
#else
	void DebugPrint(const char* format, ...) {
		va_list args;
		va_start(args, format);
		vfprintf(stderr, format, args);
		va_end(args);
	}
#endif

////////////////////////////////////////////////////////////////

template <typename Code>
struct scope_exit_implementation {
	Code code;
	bool enabled = false;

	FORCEINLINE scope_exit_implementation(Code code) : code(code), enabled(true) { }
	FORCEINLINE ~scope_exit_implementation() { if (enabled) code(); }
};

struct scope_exit_token {
	template <typename Code>
	FORCEINLINE scope_exit_implementation<Code> operator*(Code code) const { return {code}; }
};

#define scope_exit scope_exit_token{}*[&]

////////////////////////////////////////////////////////////////

enum class ReadMode {
	Silent,
	LogErrors,
};

bool ReadFile(const char* file_name, std::vector<char>& contents, ReadMode mode = ReadMode::LogErrors) {
	contents.clear();

	FILE* file = fopen(file_name, "rb");
	if (!file) {
		if (mode != ReadMode::Silent)
			fprintf(stderr, "ERROR: Could not open file '%s'.\n", file_name);
		return false;
	}
	auto close_file = scope_exit { fclose(file); file = NULL; };

	fseek(file, 0, SEEK_END);
	auto file_size = ftell(file);
	fseek(file, 0, SEEK_SET);

	contents.resize(file_size);
	if (!fread(contents.data(), file_size, 1, file)) {
		if (mode != ReadMode::Silent)
			fprintf(stderr, "ERROR: Could not read file '%s'.\n", file_name);
		return false;
	}

	return true;
}

////////////////////////////////////////////////////////////////

int CompareNoCase(std::string_view lhs, std::string_view rhs) {
	size_t i = 0, common = std::min(lhs.size(), rhs.size());
	while (i < common && std::tolower(lhs[i]) == std::tolower(rhs[i]))
		++i;
	char lhs_c = i < lhs.size() ? lhs[i] : 0;
	char rhs_c = i < rhs.size() ? rhs[i] : 0;
	return lhs_c - rhs_c;
}

std::string_view ExtractFileName(std::string_view path) {
	auto i = path.rfind('/');
	if (i != path.npos)
		path.remove_prefix(i + 1);
	i = path.rfind('\\');
	if (i != path.npos)
		path.remove_prefix(i + 1);
	
	i = path.find('.');
	if (i != path.npos)
		path.remove_suffix(path.size() - i);
	
	return path;
}

void ReplaceAll(std::string& dest, std::string_view old, std::string replacement) {
	for (size_t start = 0; start + old.size() < dest.size(); ) {
		size_t pos = dest.find(old, start);
		if (pos == dest.npos)
			break;
		dest.replace(pos, old.size(), replacement.data(), replacement.size());
		start = pos + replacement.size();
	}
}

bool StartsWith(std::string_view str, std::string_view prefix) {
	return str.size() >= prefix.size() && str.substr(0, prefix.size()) == prefix;
}

bool EndsWith(std::string_view str, std::string_view suffix) {
	return str.size() >= suffix.size() && str.substr(str.size() - suffix.size(), suffix.size()) == suffix;
}

bool RemovePrefix(std::string_view& str, std::string_view prefix) {
	if (!StartsWith(str, prefix))
		return false;
	str.remove_prefix(prefix.size());
	return true;
}

bool RemoveSuffix(std::string_view& str, std::string_view suffix) {
	if (!EndsWith(str, suffix))
		return false;
	str.remove_suffix(suffix.size());
	return true;
}

////////////////////////////////////////////////////////////////

void Snap(float& value, float inc) {
	value = round(value / inc) * inc;
}

void RemoveNegativeZero(float& value) {
	if (value == -0.f)
		value = 0.f;
}

void SnapMantissa(float& value, int bits) {
	u32& v = *reinterpret_cast<u32*>(&value);
	u32 mask = (1 << bits) - 1;
	v = (v + (mask >> 1)) & ~mask;
}

// Morton order ////////////////////////////////////////////////

/*
.F.E.D.C.B.A.9.8.7.6.5.4.3.2.1.0	0x55555555
..FE..DC..BA..98..76..54..32..10	0x33333333
....FEDC....BA98....7654....3210	0x0F0F0F0F
........FEDCBA98........76543210	0x00FF00FF
................FEDCBA9876543210	0x0000FFFF
*/

// interleaves zeroes between the bits of the supplied number
inline constexpr u32 Spread2(u32 x) {
	x &= 0x0000FFFF;
	x = (x ^ (x << 8)) & 0x00FF00FF;
	x = (x ^ (x << 4)) & 0x0F0F0F0F;
	x = (x ^ (x << 2)) & 0x33333333;
	x = (x ^ (x << 1)) & 0x55555555;
	return x;
}

// interleaves the low 16 bits of the two numbers
inline constexpr u32 Interleave2(u32 lo, u32 hi) {
	return Spread2(lo) | (Spread2(hi) << 1);
}

/*
......................9876543210
............98765..........43210
........987....65......432....10
......98..7....65....43..2....10
....9..8..7..6..5..4..3..2..1..0
*/

inline constexpr u32 Spread3(u32 x) {
	x &= 1023;
	x = (x ^ (x << 10)) & 0xF801F;
	x = (x ^ (x <<  4)) & 0xE181C3;
	x = (x ^ (x <<  2)) & 0x3218643;
	x = (x ^ (x <<  2)) & 0x9249249;
	return x;
}

inline constexpr u32 Interleave3(u32 x, u32 y, u32 z) {
	return Spread3(x) | (Spread3(y) << 1) | (Spread3(z) << 2);
}

////////////////////////////////////////////////////////////////

template <typename Signature>
class Function;

////////////////////////////////////////////////////////////////
template <typename Return, typename... Args>
class Function<Return(Args...)>
////////////////////////////////////////////////////////////////
{
public:
	template <typename Callable>
	Function(Callable callable) : m_caller(&Call<Callable>) {
		::new(&m_buffer) Callable(callable);
	}

	Function() = default;
	Function(const Function&) = default;

	Return operator () (Args... args) {
		return (*m_caller)(m_buffer, static_cast<Args&&>(args)...);
	}

	explicit operator bool() const {
		return m_caller != nullptr;
	}

private:
	using Buffer			= void*[2];
	Buffer					m_buffer;
	Return					(*m_caller)(Buffer& buffer, Args&&... args) = nullptr;

	template <typename Callable>
	static Return			Call(Buffer& buffer, Args&&... args) {
		return static_cast<Return>((*reinterpret_cast<Callable*>(&buffer))(args...));
	}
};

////////////////////////////////////////////////////////////////

template <typename Struct, typename Member>
struct by_member {
	Member Struct::*member;
	by_member(Member Struct::*ptr) : member(ptr) { }

	const Member& operator() (const Struct& s) const { return s.*member; }
	bool operator() (const Struct& lhs, const Struct& rhs) const { return lhs.*member < rhs.*member; }
};

// deduction guide
template <typename Struct, typename Member>
by_member(Member Struct::*) -> by_member<Struct, Member>;

////////////////////////////////////////////////////////////////

template <typename Comparator = std::less<void>>
struct descendingly {
	Comparator comparator;
	descendingly(Comparator c = {}) : comparator(c) { }

	template <typename Lhs, typename Rhs>
	bool operator()(const Lhs& lhs, const Rhs& rhs) const { return comparator(rhs, lhs); }
};

////////////////////////////////////////////////////////////////
template <typename Begin, typename End = Begin>
class range
////////////////////////////////////////////////////////////////
{
public:
	constexpr range(Begin begin, End end) :
		m_begin	(begin),
		m_end	(end)
	{ }

	constexpr range() = default;

	using iterator = Begin;

	constexpr Begin				begin() const						{ return m_begin; }
	constexpr End				end() const							{ return m_end; }
	constexpr bool				empty() const						{ return m_begin == m_end; }

	constexpr decltype(auto)	front() const						{ assert(!empty()); return *m_begin; }
	constexpr decltype(auto)	back() const						{ assert(!empty()); return *--End(m_end); }

	constexpr size_t			size() const						{ return m_end - m_begin; }
	constexpr decltype(auto)	operator[] (size_t index) const		{ assert(index < size()); return m_begin[index]; }
	constexpr decltype(auto)	operator[] (size_t index)			{ assert(index < size()); return m_begin[index]; }

protected:
	Begin						m_begin;
	End							m_end;
};

////////////////////////////////////////////////////////////////
template <typename T>
class array_view : public range<T*>
////////////////////////////////////////////////////////////////
{
	using super = range<T*>;
public:
	constexpr array_view(T* begin, size_t count) :
		super(begin, begin + count)
	{ }

	template <size_t Count>
	constexpr array_view(T (&arr)[Count]) :
		super(arr, arr + Count)
	{ }

	constexpr array_view() = default;

	constexpr T* data() { return super::begin(); }
	constexpr const T* data() const { return super::begin(); }
};

////////////////////////////////////////////////////////////////

template <typename T>
inline size_t HashValue(const T& val) {
	return std::hash<T>{}(val);
}

template <typename T>
inline size_t HashCombine(size_t seed, const T& val) {
	return seed ^ (std::hash<T>{}(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

struct PairHasher {
	template <typename A, typename B>
	inline size_t operator()(const std::pair<A, B>& p) const {
		return HashCombine(std::hash<A>{}(p.first), p.second);
	}
};

////////////////////////////////////////////////////////////////

void WriteVarint(ArrayPrinter& print, u32 v) {
	do {
		print << i32((v & 127u) | (u8(v >= 128) << 7)) << ","sv;
		v >>= 7;
	} while (v);
}

void WriteVarint(std::vector<u8>& data, u32 v) {
	do {
		data.push_back(i32((v & 127u) | (u8(v >= 128) << 7)));
		v >>= 7;
	} while (v);
}
