#pragma once

struct ArrayPrinter {
	static const size_t		buffer_size = 4096;
	char					buffer[buffer_size];
	size_t					line_size = 128;
	size_t					filled = 0;
	const char*				suffix = nullptr;
	const char*				prefix = nullptr;
	FILE*					file = nullptr;
	size_t					suffix_length = 0;
	size_t					prefix_length = 0;

	ArrayPrinter(FILE* f, int line_size = 128) :
		file(f),
		line_size(line_size) {
	}

	~ArrayPrinter() {
		Flush();
	}

	ArrayPrinter& SetPrefix(const char* str, size_t length) {
		prefix = str;
		prefix_length = int(length);
		return *this;
	}

	ArrayPrinter& SetSuffix(const char* str, size_t length) {
		suffix = str;
		suffix_length = int(length);
		return *this;
	}

	ArrayPrinter& SetPrefix(string_view str) {
		return SetPrefix(str.data(), str.size());
	}

	ArrayPrinter& SetSuffix(string_view str) {
		return SetSuffix(str.data(), str.size());
	}

	enum class FlushMode {
		Manual,
		Auto,
	};

	ArrayPrinter& Flush(FlushMode mode = FlushMode::Manual) {
		if (filled > 0) {
			fprintf(file, "%.*s%.*s\n%.*s", int(filled), buffer, int(suffix_length), suffix, mode==FlushMode::Manual? 0 : int(prefix_length), prefix);
			filled = 0;
		}
		return *this;
	}

	ArrayPrinter& operator<<(string_view str) {
		if (str.size() + filled > line_size)
			Flush(FlushMode::Auto);
		if (str.size() > buffer_size) {
			fprintf(file, "%.*s\n", int(str.size()), str.data());
		} else {
			memcpy(&buffer[filled], str.data(), str.size());
			filled += str.size();
		}
		return *this;
	}

	ArrayPrinter& operator<<(uint32_t u) {
		char str_hex[32];
		int hex = sprintf(str_hex, "0x%xu", u);
		char str_dec[32];
		int dec = sprintf(str_dec, "%uu", u);
		return *this << (hex <= dec ? str_hex : str_dec);
	}

	ArrayPrinter& operator<<(int32_t u) {
		int shift = 0;
		if (u > 0)
			for (auto low_bit=(u&-u)>>1; low_bit; low_bit>>=1)
				++shift;
		bool use_shift = true;
		use_shift &= (shift >= 13);
		if (use_shift)
			u >>= shift;
		char str_hex[32];
		int hex = sprintf(str_hex, "0x%x", u);
		char str_dec[32];
		int dec = sprintf(str_dec, "%d", u);
		*this << (hex <= dec ? str_hex : str_dec);
		if (use_shift) {
			*this << "<<"sv;
			dec = sprintf(str_dec, "%d", shift);
			*this << str_dec;
		}
		return *this;
	}

	ArrayPrinter& operator<<(float f) {
		if (f < 0.f) {
			(*this) << "-";
			f = -f;
		}
		char str_dec[32];
		int dec = sprintf(str_dec, "%g", f);
		char* p = str_dec;
		if (p[0] == '0' && p[1] == '.')
			memmove(p, p+1, strlen(p));
		return *this << str_dec;
	}
};
