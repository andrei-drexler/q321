#pragma once

#include "arena.h"

constexpr inline bool IsAlphanumeric(char c) {
	return ((unsigned)(c - '0') <= 9) | ((unsigned)(c - 'a') <= 'z' - 'a') | ((unsigned)(c - 'A') <= 'Z' - 'A');
}

struct AtomList {
	using Map		= std::unordered_map<std::string_view, const char*>;

	const char*		Intern(std::string_view string);

	Arena			m_arena;
	Map				m_ident_map;
};

////////////////////////////////////////////////////////////////

struct Lexer {
	using Atom			= const char*;

	struct Token {
		enum Type {
			Invalid = -1,
			EndOfStream = 0,

			Plus = '+',
			Minus = '-',
			Times = '*',
			Divided = '/',
			Mod = '%',

			Or = '|',
			And = '&',
			Xor = '^',

			Assign = '=',

			CmpGreater = '>',
			CmpLess = '<',

			Member = '.',
			Ternary = '?',

			Directive = '#',
			EndOfLine = '\n',

			BraceOpen = '{',
			BraceClose = '}',
			ParenOpen = '(',
			ParenClose = ')',
			SquareBracketOpen = '[',
			SquareBracketClose = ']',
			Comma = ',',
			Semicolon = ';',
			Colon = ':',

			Constant = 256,
			Identifier,

			ShiftRight,
			ShiftLeft,

			CmpEquals,
			CmpNotEquals,
			CmpGreaterOrEqual,
			CmpLessOrEqual,

			LogicalAnd,
			LogicalOr,

			Increment,
			Decrement,
			PlusEquals,
			MinusEquals,
			TimesEquals,
			DividedEquals,
			ModEquals,
			OrEquals,
			AndEquals,
			XorEquals,
			ShiftRightEquals,
			ShiftLeftEquals,

			Count,
		} type = Type::Invalid;

		Atom value = {};

		string_view ToString() const;
	};

	void					SetSource(string_view source);
	bool					IsDone() const						{ return m_token.type <= Token::Type::EndOfStream; }
	const Token&			Peek()								{ return m_token; }
	void					Consume()							{ FetchToken(); }

	bool					Match(Token::Type type)				{ if (m_token.type != type) return false; Consume(); return true; }

	Atom					Intern(const char* s)				{ return m_atom_list.Intern(s); }
	AtomList&				GetAtoms()							{ return m_atom_list; }

private:
	void					FetchToken();

	enum class CommentType {
		SingleLine,
		MultiLine,
	};

	void					SkipWhitespace();
	void					SkipComment(CommentType type);

	void					FetchNumber();
	bool					FetchIdentifier();

	size_t					CharsLeft() const					{ return m_end - m_cursor; }
	char					PeekChar(int i = 0) const			{ return m_cursor[i]; }
	bool					MatchChar(char c)					{ if (!CharsLeft() || PeekChar() != c) return false; ConsumeChar(); return true; }
	void					ConsumeChar()						{ m_cursor += (m_cursor != m_end); }

	const char*				m_source = nullptr;
	const char*				m_cursor = nullptr;
	const char*				m_end = nullptr;
	const char*				m_line_start = nullptr;
	int						m_current_line = 1;
	bool					m_directive = false;
	Token					m_token;
	Arena					m_arena;
	AtomList				m_atom_list;
};

////////////////////////////////////////////////////////////////
// Implementation //////////////////////////////////////////////
////////////////////////////////////////////////////////////////

const char* AtomList::Intern(std::string_view string) {
	auto i = m_ident_map.find(string);
	if (i == m_ident_map.end()) {
		size_t size = string.size();
		auto buffer = m_arena.alloc(size + 1);
		memcpy(buffer, string.data(), size);
		buffer[size] = 0;
		const char* copy = (const char*)buffer;
		i = m_ident_map.insert({copy, copy}).first;
	}
	return i->second;
}

////////////////////////////////////////////////////////////////

string_view Lexer::Token::ToString() const {
	if (type <= Type::EndOfStream)
		return ""sv;

	if (type < Type::Constant)
		return {(const char*)&type, 1}; // Note: little-endian only!

	switch (type) {
		case Type::Constant:
		case Type::Identifier:
			return value;

		case Type::ShiftRight:			return ">>"sv;
		case Type::ShiftLeft:			return "<<"sv;

		case Type::CmpEquals:			return "=="sv;
		case Type::CmpNotEquals:		return "!="sv;
		case Type::CmpGreaterOrEqual:	return ">="sv;
		case Type::CmpLessOrEqual:		return "<="sv;

		case Type::LogicalAnd:			return "&&"sv;
		case Type::LogicalOr:			return "||"sv;

		case Type::Increment:			return "++"sv;
		case Type::Decrement:			return "--"sv;
		case Type::PlusEquals:			return "+="sv;
		case Type::MinusEquals:			return "-="sv;
		case Type::TimesEquals:			return "*="sv;
		case Type::DividedEquals:		return "/="sv;
		case Type::ModEquals:			return "%="sv;
		case Type::OrEquals:			return "|="sv;
		case Type::AndEquals:			return "&="sv;
		case Type::XorEquals:			return "^="sv;
		case Type::ShiftRightEquals:	return ">>="sv;
		case Type::ShiftLeftEquals:		return "<<="sv;

		default:
			return ""sv;
	}
}

////////////////////////////////////////////////////////////////

void Lexer::SetSource(string_view source) {
	m_source = source.data();
	m_cursor = m_source;
	m_end = m_source + source.size();

	m_line_start = m_cursor;
	m_current_line = 1;

	FetchToken();
}

////////////////////////////////////////////////////////////////

void Lexer::FetchToken() {
beginning:
	if (!CharsLeft()) {
		m_token.type = Token::Type::EndOfStream;
		return;
	}

	m_token.type = Token::Type::Invalid;
	m_token.value = nullptr;

	char current = PeekChar();
	size_t left = CharsLeft();
	switch (current) {
		case ' ': case '\t': case '\n': case '\r': case '\v':
			SkipWhitespace();
			if (m_token.type == Token::Type::EndOfLine)
				return;
			goto beginning;

		case '\\':
			ConsumeChar();
			while (CharsLeft() && PeekChar() == '\r')
				ConsumeChar();
			if (!MatchChar('\n'))
				return;
			SkipWhitespace();
			if (m_token.type == Token::Type::EndOfLine)
				return;
			goto beginning;

		case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
			FetchNumber();
			return;

		case '.':
			if (left < 1 || !isdigit(PeekChar(1))) {
				ConsumeChar();
				m_token.type = (Token::Type)current;
			} else {
				FetchNumber();
			}
			return;

		case '_':
		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
		case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
		case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
		case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
		case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
			FetchIdentifier();
			return;

		case ';':
		case ',':
		case '(':
		case ')':
		case '[':
		case ']':
		case '{':
		case '}':
		case '?':
		case ':':
		case '#':
			ConsumeChar();
			if (current == '#')
				m_directive = true;
			m_token.type = (Token::Type)current;
			return;

		case '<':
			ConsumeChar();
			if (MatchChar('<')) {
				if (MatchChar('='))
					m_token.type = Token::Type::ShiftLeftEquals;
				else
					m_token.type = Token::Type::ShiftLeft;
			} else if (MatchChar('=')) {
				m_token.type = Token::Type::CmpLessOrEqual;
			} else {
				m_token.type = Token::Type::CmpLess;
			}
			return;

		case '>':
			ConsumeChar();
			if (MatchChar('>')) {
				if (MatchChar('='))
					m_token.type = Token::Type::ShiftRightEquals;
				else
					m_token.type = Token::Type::ShiftRight;
			} else if (MatchChar('=')) {
				m_token.type = Token::Type::CmpGreaterOrEqual;
			} else {
				m_token.type = Token::Type::CmpGreater;
			}
			return;

		case '/':
			ConsumeChar();
			if (MatchChar('=')) {
				m_token.type = Token::Type::DividedEquals;
			} else if (MatchChar('*')) {
				SkipComment(CommentType::MultiLine);
				goto beginning;
			} else if (MatchChar('/')) {
				SkipComment(CommentType::SingleLine);
				goto beginning;
			} else {
				m_token.type = (Token::Type)'/';
			}
			return;

#define CASE_12(c0, c1, t1)							\
		case c0:									\
			ConsumeChar();							\
			if (MatchChar(c1))						\
				m_token.type = Token::Type::t1;		\
			else									\
				m_token.type = (Token::Type)c0;		\
			return									\

#define CASE_122(c0, c1a, t1a, c1b, t1b)			\
		case c0:									\
			ConsumeChar();							\
			if (MatchChar(c1a))						\
				m_token.type = Token::Type::t1a;	\
			else if (MatchChar(c1b))				\
				m_token.type = Token::Type::t1b;	\
			else									\
				m_token.type = (Token::Type)c0;		\
			return									\

		CASE_12	('=', '=', CmpEquals);
		CASE_12	('*', '=', TimesEquals);
		CASE_12	('%', '=', ModEquals);
		CASE_12	('^', '=', XorEquals);
		CASE_122('+', '+', Increment,		'=', PlusEquals);
		CASE_122('-', '-', Decrement,		'=', MinusEquals);
		CASE_122('&', '&', LogicalAnd,		'=', AndEquals);
		CASE_122('|', '|', LogicalOr,		'=', OrEquals);

		default:
			break;
	}
}

////////////////////////////////////////////////////////////////

void Lexer::SkipWhitespace() {
	while (CharsLeft()) {
		char c = PeekChar();
		if (!IsWhitespace(c))
			break;
		ConsumeChar();
		if (c == '\n') {
			m_line_start = m_cursor;
			if (m_directive) {
				m_token.type = Token::Type::EndOfLine;
				m_directive = false;
			}
			++m_current_line;
		}
	}
}

////////////////////////////////////////////////////////////////

void Lexer::SkipComment(CommentType type) {
	if (type == CommentType::SingleLine) {
		while (m_cursor != m_end && *m_cursor != '\n' && *m_cursor != '\r')
			++m_cursor;
	} else {
		while (m_cursor != m_end) {
			while (m_cursor != m_end && *m_cursor != '*')
				++m_cursor;
			if (m_cursor == m_end)
				break;
			if (m_cursor[1] == '/') {
				m_cursor += 2;
				break;
			}
			++m_cursor;
		}
	}
}

////////////////////////////////////////////////////////////////

void Lexer::FetchNumber() {
	const char* begin = m_cursor;
	while (CharsLeft() && isdigit(PeekChar()))
		ConsumeChar();

	bool is_float = false;
	if (CharsLeft() && PeekChar() == '.') {
		ConsumeChar();
		is_float = true;
		while (isdigit(PeekChar()))
			ConsumeChar();
	}

	if (m_cursor == begin || (m_cursor == begin + 1 && *begin == '.'))
		return;

	/* consume suffix, if any */
	if (CharsLeft()) {
		char next = PeekChar();
		if (next == 'e' || next == 'E') {
			/* scientific notation */
			ConsumeChar();
			if (CharsLeft()) {
				next = PeekChar();
				if (next == '+' || next == '-') 
					ConsumeChar();
				while (CharsLeft() && isdigit(PeekChar()))
					ConsumeChar();
			}
		} else if (is_float) {
			if (next == 'f' || next == 'F')
				ConsumeChar();
		} else {
			if (next == 'u' || next == 'U')
				ConsumeChar();
		}
	}

	m_token.type = Token::Type::Constant;
	m_token.value = m_atom_list.Intern({begin, (size_t)(m_cursor - begin)});
}

////////////////////////////////////////////////////////////////

bool Lexer::FetchIdentifier() {
	auto begin = m_cursor;
	ConsumeChar();

	while (CharsLeft()) {
		char c = PeekChar();
		if (!IsAlphanumeric(c) && c != '_') 
			break;
		ConsumeChar();
	}

	std::string_view name{begin, size_t(m_cursor - begin)};
	if (name.empty())
		return false;

	m_token.type = Token::Type::Identifier;
	m_token.value = m_atom_list.Intern(name);

	return true;
}
