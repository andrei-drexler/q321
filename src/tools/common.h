#pragma once

#define _CRT_SECURE_NO_WARNINGS 1

#include <cstdio>
#include <cstdint>
#include <cstdarg>
#include <cassert>
#include <climits>
#include <limits>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <charconv>
#include <algorithm>

using namespace std::string_view_literals;
using std::string_view;

////////////////////////////////////////////////////////////////

#include "../../engine/common.h"
#include "../../engine/math.h"
#include "../../engine/aabb.h"

#include "parse.h"

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

bool ReadFile(const char* file_name, std::vector<char>& contents) {
	contents.clear();

	FILE* file = fopen(file_name, "rb");
	if (!file) {
		fprintf(stderr, "ERROR: Could not open file '%s'.\n", file_name);
		return false;
	}
	auto close_file = scope_exit { fclose(file); file = NULL; };

	fseek(file, 0, SEEK_END);
	auto file_size = ftell(file);
	fseek(file, 0, SEEK_SET);

	contents.resize(file_size);
	if (!fread(contents.data(), file_size, 1, file)) {
		fprintf(stderr, "ERROR: Could not read file '%s'.\n", file_name);
		return false;
	}

	return true;
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

	constexpr decltype(auto)	front() const						{ return *m_begin; }
	constexpr decltype(auto)	back() const						{ return *--End(m_end); }

	constexpr decltype(auto)	operator[] (size_t index) const		{ return m_begin[index]; }
	constexpr decltype(auto)	operator[] (size_t index)			{ return m_begin[index]; }
	constexpr size_t			size() const						{ return m_end - m_begin; }

protected:
	Begin						m_begin;
	End							m_end;
};

////////////////////////////////////////////////////////////////
