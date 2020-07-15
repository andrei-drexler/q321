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
	#pragma intrinsic(__stosd)

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

#define PP_STRINGIZE_(x)		#x
#define PP_STRINGIZE(x)			PP_STRINGIZE_(x)
#define PP_EXPAND(x)			x
#define PP_FIRST_ARG2(x,...)	x
#define PP_FIRST_ARG(x,...)		PP_EXPAND(PP_FIRST_ARG2(x))

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

#if !defined(_MSC_VER) || defined(__clang__)
	#define OFFSET_OF(type, member) __builtin_offsetof(type, member)
#else
	#define OFFSET_OF(type, member) ((size_t)(&((type*)1)->member) - 1)
#endif

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

FORCEINLINE void* MemSet32(void* dest, i32 value, size_t count) {
	#ifdef _MSC_VER
		__stosd((unsigned long*)dest, (unsigned long)value, count);
	#else
		i32* dwords = (i32*)dest;
		while (count--)
			*dwords++ = value;
		return dest;
	#endif
	return dest;
}

template <typename T>
FORCEINLINE T* MemSet(T* dest, int c = 0, size_t count = 1) {
	MemSet((void*)dest, c, count * sizeof(T));
	return dest;
}

NOINLINE constexpr size_t StrLen(const char* text) {
	size_t result = 0;
	while (text[result])
		++result;
	return result;
}

FORCEINLINE constexpr int StrCmp(const char* lhs, const char* rhs) {
	while (*lhs && *rhs && *lhs == *rhs)
		++lhs, ++rhs;
	return *lhs - *rhs;
}

FORCEINLINE size_t CopyString(char* dst, const char* src) {
	size_t result = StrLen(src);
	MemCopy(dst, src, result + 1);
	return result;
}

FORCEINLINE constexpr const char* NextAfter(const char* multistr) {
	return multistr + StrLen(multistr) + 1;
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

NOINLINE char* FloatToString(float f, char* out) {
	u32 bits = *(u32*)&f;

	if (bits & 0x8000'0000) {
		bits ^= 0x8000'0000;
		*out++ = '-';
	}

	i32 exp2		= i32((bits >> 23) & 0xFF) - 127;
	u32 mantissa	= (bits & 0x00FF'FFFF) | 0x0080'0000;

	if (exp2 >= 31) {
		*out++ = 'I';
		*out++ = 'N';
		*out++ = 'F';
		return out;
	}

	u32 int_part, frac_part;
	if (exp2 >= 23) {
		int_part = mantissa << (exp2 - 23);
		frac_part = 0;
	} else if (exp2 >= 0) {
		int_part = mantissa >> (23 - exp2);
		frac_part = (mantissa << (exp2 + 1)) & 0x00FF'FFFF;
	} else if (exp2 >= -23) {
		int_part = 0;
		frac_part = (mantissa & 0x00FF'FFFF) >> -(exp2 + 1);
	} else {
		int_part = 0;
		frac_part = 0;
	}

	out = IntToString(int_part, out);
	*out++ = '.';

	const u16 NumDecimals = 8;
	for (u16 i = 0; i < NumDecimals; ++i) {
		frac_part *= 10;
		*out++ = '0' + (frac_part >> 24);
		frac_part &= 0x00FF'FFFF;
	}

	*out = '\0';

	return out;
}

NOINLINE float StringToFloat(const char* text, iptr* consumed = 0) {
	const char* start = text;
	while (*text == ' ' || *text == '\t')
		++text;
	iptr spaces = text - start;

	char c = *text;
	bool negative = (c == '-');
	bool has_sign = ((c == '+') | negative);
	text += has_sign;
	i32 i = 0;
	while (u8(*text - '0') < 10)
		i = i * 10 + (*text++ - '0');

	float result = float(i);
	bool has_dot = (*text == '.');
	if (has_dot) {
		++text;
		i = 0;
		float denom = 1.f;
		while (u8(*text - '0') < 10) {
			i = i * 10 + (*text++ - '0');
			denom *= 10.f;
		}
		result += i / denom;
	}

	if (consumed) {
		iptr dist = text - start;
		*consumed = (dist > spaces + has_sign + has_dot) ? dist : 0;
	}

	return negative ? -result : result;
}

////////////////////////////////////////////////////////////////

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

NOINLINE constexpr u32 EncodeSignMagnitude(i32 i) {
	u32 s = u32(i) >> 31;
	return (u32((i ^ -i32(s)) + s) << 1) | s;
}

NOINLINE constexpr i32 DecodeSignMagnitude(u32 u) {
	bool negative = u & 1;
	u >>= 1;
	return negative ? -i32(u) : i32(u);
}

////////////////////////////////////////////////////////////////

template <typename T, size_t Size>
struct StaticArray {
	T data[Size];

	constexpr T&							operator[](size_t i)									{ assert(i < Size); return data[i]; }
	constexpr const T&						operator[](size_t i) const								{ assert(i < Size); return data[i]; }

	constexpr explicit operator T*			()														{ return data; }
	constexpr explicit operator const T*	() const												{ return data; }
};

// Note: we allow pointers to one past last item. Bad idea?
template <typename T, size_t Size>
static constexpr T* operator+(StaticArray<T, Size>& array, size_t offset) {
	assert(offset <= Size);
	return array.data + offset;
}

template <typename T, size_t Size>
static constexpr T* operator+(size_t offset, StaticArray<T, Size>& array) {
	assert(offset <= Size);
	return array.data + offset;
}

template <typename T, size_t Size>
static constexpr const T* operator+(const StaticArray<T, Size>& array, size_t offset) {
	assert(offset <= Size);
	return array.data + offset;
}

template <typename T, size_t Size>
static constexpr const T* operator+(size_t offset, const StaticArray<T, Size>& array) {
	assert(offset <= Size);
	return array.data + offset;
}

// Note: non-member size() to allow the same code to work with both built-in arrays and our custom type
template <typename T, size_t Size>
static constexpr size_t size(const StaticArray<T, Size>&) {
	return Size;
}

#ifdef DEV
	template <typename T, size_t Size>
	using Array = StaticArray<T, Size>;		// Bounds-checked (DEV build)
#else
	template <typename T, size_t Size>
	using Array = T[Size];					// Raw (no bounds checking)
#endif

////////////////////////////////////////////////////////////////

template <size_t Size, typename Value, typename Key = size_t, Key First = Key{}>
struct LookupTable {
	Value data[Size];

	static constexpr size_t		size()						{ return Size; }
	constexpr Value&			operator[](Key key) {
		size_t i = size_t(key) - size_t(First);
		if (i >= Size - 1)
			i = Size - 1;
		return data[i];
	}
	
	constexpr const Value&		operator[](Key key) const {
		size_t i = size_t(key) - size_t(First);
		if (i >= Size - 1)
			i = Size - 1;
		return data[i];
	}
};

template <typename Key, typename Value, Key FirstKey, Key LastKey, typename MapFunction>
constexpr auto MakeLookupTable(MapFunction map) {
	LookupTable<size_t(LastKey) - size_t(FirstKey) + 2, Value, Key, FirstKey> result = {};
	for (size_t i = 0; i < result.size(); ++i)
		result.data[i] = map(static_cast<Key>(static_cast<size_t>(FirstKey) + i));
	return result;
}

