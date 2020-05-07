#pragma once

////////////////////////////////////////////////////////////////

bool IsWhitespace(char c) {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

void SkipWhitespace(string_view& s) {
	while (!s.empty()) {
		while (!s.empty() && IsWhitespace(s[0]))
			s.remove_prefix(1);
		if (s.size() < 2 || s[0] != '/' || s[1] != '/')
			return;
		auto end = s.find('\n');
		if (end == s.npos) {
			s = {};
			return;
		}
		s.remove_prefix(end + 1);
	}
}

bool Match(string_view& source, string_view text) {
	SkipWhitespace(source);
	if (source.length() >= text.length() && memcmp(source.data(), text.data(), text.length()) == 0) {
		source.remove_prefix(text.length());
		return true;
	}
	return false;
}

template <typename T>
bool Parse(string_view& source, T& value) {
	SkipWhitespace(source);
	std::from_chars_result res;
	if constexpr (std::is_enum_v<T>) {
		std::underlying_type_t<T> tmp;
		res = std::from_chars(source.data(), source.data() + source.size(), tmp);
		value = static_cast<T>(tmp);
	} else {
		res = std::from_chars(source.data(), source.data() + source.size(), value);
	}
	if (res.ec != std::errc())
		return false;
	source.remove_prefix(res.ptr - source.data());
	return true;
}

bool Parse(string_view& source, vec3& v, bool parentheses = true) {
	auto backup = source;
	if (parentheses && !Match(source, "("sv))
		return false;
	
	for (int i=0; i<3; ++i) {
		if (!Parse(source, v.data[i])) {
			source = backup;
			return false;
		}
	}
	
	if (parentheses && !Match(source, ")"sv)) {
		source = backup;
		return false;
	}

	return true;
}

template <typename T, typename... ExtraArgs>
bool Parse(const string_view& source, T& value, ExtraArgs&&... args) {
	auto copy = source;
	return Parse(copy, value, static_cast<ExtraArgs&&>(args)...);
}

string_view ParseWord(string_view& source) {
	SkipWhitespace(source);
	size_t cursor = 0, length = source.length();
	while (cursor < length && !IsWhitespace(source[cursor]))
		++cursor;
	auto result = source.substr(0, cursor);
	source.remove_prefix(cursor);
	return result;
}

string_view ParseQuotedString(string_view& source) {
	SkipWhitespace(source);
	auto backup = source;
	if (source.empty() || source[0] != '\"')
		return {};
	auto end = source.find('\"', 1);
	if (end == source.npos)
		return {};
	auto output = source.substr(0, end + 1);
	source.remove_prefix(end + 1);
	return output;
}

