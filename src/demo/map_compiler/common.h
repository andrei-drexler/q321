#pragma once

#define _CRT_SECURE_NO_WARNINGS 1

#include <cstdio>
#include <cstdint>
#include <cstdarg>
#include <cassert>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
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
