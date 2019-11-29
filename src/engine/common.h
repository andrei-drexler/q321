#pragma once

//#pragma optimize("", off)

#ifdef _MSC_VER
	#include <intrin.h>

	using u8 = unsigned char;
	using i8 = signed char;
	using u16 = unsigned short;
	using i16 = signed short;
	using u32 = unsigned int;
	using i32 = signed int;
	using u64 = unsigned long long;
	using i64 = signed long long;
	using f32 = float;
	using f64 = double;

	#ifndef FORCEINLINE
		#define FORCEINLINE					__forceinline
	#endif
	
	#ifndef NOINLINE
		#define NOINLINE					__declspec(noinline)
	#endif

	#define PP_CPP_VERSION					_MSVC_LANG

	#ifndef PP_USE_INLINE_ASM
		#if !defined(_M_X64)
			#define PP_USE_INLINE_ASM		1
		#else
			#define PP_USE_INLINE_ASM		0
		#endif
	#endif

	#pragma warning(error: 4002)			// too many actual parameters for macro 'identifier'
	#pragma warning(error: 4003)			// not enough actual parameters for macro 'identifier'

	#pragma intrinsic(__movsb)
	#pragma intrinsic(__stosb)

#else
	#define PP_CPP_VERSION					__cplusplus
	#define PP_USE_INLINE_ASM				0

	#ifndef FORCEINLINE
		#define FORCEINLINE					__forceinline
	#endif
	
	#ifndef NOINLINE
		#define NOINLINE					__declspec(noinline)
	#endif

	#error Unsupported platform
#endif

#define PP_STRINGIZE_(x)	#x
#define PP_STRINGIZE(x)		PP_STRINGIZE_(x)

////////////////////////////////////////////////////////////////

static_assert(sizeof(u8)  == 1, "Invalid u8 typedef");
static_assert(sizeof(i8)  == 1, "Invalid i8 typedef");
static_assert(sizeof(u16) == 2, "Invalid u16 typedef");
static_assert(sizeof(i16) == 2, "Invalid i16 typedef");
static_assert(sizeof(u32) == 4, "Invalid u32 typedef");
static_assert(sizeof(i32) == 4, "Invalid i32 typedef");
static_assert(sizeof(u64) == 8, "Invalid u64 typedef");
static_assert(sizeof(i64) == 8, "Invalid i64 typedef");
static_assert(sizeof(f32) == 4, "Invalid f32 typedef");
static_assert(sizeof(f64) == 8, "Invalid f64 typedef");

////////////////////////////////////////////////////////////////

namespace details {
	template <int Size> struct SizedType;

	template <>
	struct SizedType<sizeof(u32)> {
		using signed_type	= i32;
		using unsigned_type	= u32;
	};

	template <>
	struct SizedType<sizeof(u64)> {
		using signed_type	= i64;
		using unsigned_type	= u64;
	};
} // namespace details

template <int Size> using int_sized  = typename details::SizedType<Size>::signed_type;
template <int Size> using uint_sized = typename details::SizedType<Size>::unsigned_type;

using iptr = int_sized<sizeof(void*)>;
using uptr = uint_sized<sizeof(void*)>;

////////////////////////////////////////////////////////////////

FORCEINLINE void* MemCopy(void* dest, const void* src, size_t count) {
	#ifdef _MSC_VER
		__movsb((unsigned char*)dest, (const unsigned char*)src, count);
	#else
		char* dest_bytes = (char*)dest;
		const char* src_bytes = (const char*)src;
		while (count--)
			*dest_bytes++ = *src_bytes++;
	#endif
	return dest;
}

template <typename T>
FORCEINLINE T* MemCopy(T* dest, const T* src, size_t count = 1) {
	MemCopy((void*)dest, (const void*)src, count * sizeof(T));
	return dest;
}

FORCEINLINE void* MemSet(void* dest, int c, size_t count) {
	#ifdef _MSC_VER
		__stosb((unsigned char*)dest, (unsigned char)c, count);
	#else
		char* bytes = (char*)dest;
		while (count--)
			*bytes++ = (char)c;
		return dest;
	#endif
	return dest;
}

template <typename T>
FORCEINLINE T* MemSet(T* dest, int c = 0, size_t count = 1) {
	MemSet((void*)dest, c, count * sizeof(T));
	return dest;
}

FORCEINLINE size_t StrLen(const char* text) {
	const char* s = text;
	while (*s)
		++s;
	return s - text;
}

////////////////////////////////////////////////////////////////

template <typename T>
FORCEINLINE void Swap(T& a, T& b) {
	T tmp = static_cast<T&&>(a);
	a = static_cast<T&&>(b);
	b = static_cast<T&&>(tmp);
}

template <typename T, typename Less>
void SimpleSort(T* data, u32 count, Less less) {
	for (u32 i = 0; i + 1 < count; ++i) {
		for (u32 j = i + 1; j < count; ++j)
			if (less(data[j], data[i]))
				Swap(data[j], data[i]);
	}
}

////////////////////////////////////////////////////////////////

NOINLINE char* IntToString(i32 i, char* out) {
	if (i < 0) {
		*out++ = '-';
		i = -i;
	}

	u32 u = i;
	u16 count = 0;
	do {
		++count;
		u /= 10;
	} while (u);

	out[count] = 0;

	u = i;
	u16 p = count;
	do {
		out[--p] = u % 10 + '0';
		u /= 10;
	} while (p);

	return out + count;
}

NOINLINE constexpr const char* NextAfter(const char* multistr) {
	if (*multistr) {
		while (*multistr)
			++multistr;
		++multistr;
	}
	return multistr;
}

constexpr u32 HashAppend(u32 partial, char c) {
	// Paul Larson's multiplicative hash
	partial = partial * 101 + (unsigned char)c;
	return partial;
}

constexpr u32 Hash(const char* text) {
	u32 ret = 0;
	while (*text)
		ret = HashAppend(ret, *text++);
	return ret;
}

////////////////////////////////////////////////////////////////

u32 g_random_seed;

FORCEINLINE void Seed(u32 seed) {
	g_random_seed = seed;
}

NOINLINE u32 Random() {
	u32 r = g_random_seed * 0x45d9f3b;
	g_random_seed = r;

	// xorshift step
	r ^= r << 13;
	r ^= r >> 17;
	r ^= r << 5;

	return r;
}

////////////////////////////////////////////////////////////////

FORCEINLINE u32 SelectBits(u32 condition, u32 true_value, u32 false_value) {
	return false_value ^ ((true_value ^ false_value) & condition);
}

////////////////////////////////////////////////////////////////

template <size_t Size, typename T>
struct FixedArray {
	T data[Size];

	static constexpr size_t		size()						{ return Size; }
	constexpr T&				operator[](size_t i)		{ return data[i]; }
	constexpr const T&			operator[](size_t i) const	{ return data[i]; }
};

template <size_t Size, typename Value, typename Key = size_t, Key First = Key{}>
struct LookupTable {
	Value data[Size];

	static constexpr size_t		size()						{ return Size; }
	constexpr Value&			operator[](Key key) {
		size_t i = size_t(key - First);
		if (i >= Size - 1)
			i = Size - 1;
		return data[i];
	}
	
	constexpr const Value&		operator[](Key key) const {
		size_t i = size_t(key - First);
		if (i >= Size - 1)
			i = Size - 1;
		return data[i];
	}
};

template <typename Key, typename Value, Key FirstKey, Key LastKey, typename MapFunction>
constexpr auto MakeLookupTable(MapFunction map) {
	LookupTable<size_t(LastKey - FirstKey + 2), Value, Key, FirstKey> result = {};
	for (size_t i=0; i<result.size(); ++i)
		result.data[i] = map(static_cast<Key>(static_cast<size_t>(FirstKey) + i));
	return result;
}

