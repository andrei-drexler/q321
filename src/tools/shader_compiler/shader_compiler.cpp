#include "../common.h"
#include "lexer.h"
#include "../../demo/resource_def.h"
#include <filesystem>
#include <chrono>

using Token = Lexer::Token;
namespace fs = std::filesystem;

#define INDENT "   "

////////////////////////////////////////////////////////////////

struct Options {
	std::string output_path;
	std::string source_path;
};

bool GatherOptions(Options& options, int argc, const char** argv) {
	if (argc == 1) {
		options.source_path = "../../demo/data/shaders";
		options.output_path = "../../demo/cooked/cooked_shaders.h";

		printf(
			"No arguments specified, using default paths:\n"
			"  source_folder: %s\n"
			"  output_file:   %s\n"
			"\n",
			options.source_path.c_str(),
			options.output_path.c_str()
		);
	} else if (argc < 3) {
		printf("Usage: %s source_folder output_file\n\n",
			fs::path(argv[0]).filename().string().c_str());
		return false;
	} else {
		options.source_path = argv[1];
		options.output_path = argv[2];
	}

	return true;
}

////////////////////////////////////////////////////////////////

static constexpr string_view Keywords[] = {
	"void","float","int","uint","bool","vec2","ivec2","uvec2","bvec2","vec3","ivec3","uvec3","bvec3","vec4","ivec4","uvec4","bvec4",
	"struct","mat2","mat3","mat4","mat2x2","mat2x3","mat2x4","mat3x2","mat3x3","mat3x4","mat4x2","mat4x3","mat4x4","sampler2D",
	"sampler3D","samplerCube","sampler2DShadow","samplerCubeShadow","sampler2DArray","uniform","const","in","out","inout","layout",
	"location","main","if","else","for","while","do","switch","case","default","break","continue","return","discard",
	"texture","textureLod","textureProj","texelFetch","textureGrad","textureSize","floor","round","fract","mod","sin","asin","cos",
	"acos","tan","atan","radians","degrees","mix","smoothstep","step","abs","sign","max","min","clamp","reflect","normalize","length","sqrt", "inversesqrt",
	"pow","exp","exp2","log","log2","dot","fwidth","dFdx","dFdy",
	"any","all","not","equal","notEqual","lessThan","lessThanEqual","greaterThan","greaterThanEqual",
	"gl_Position","gl_FragCoord",
};

static constexpr string_view Uniforms[] = {
	#define PP_ADD_UNIFORM(name, ...) #name##sv,
	DEMO_UNIFORMS(PP_ADD_UNIFORM)
	#undef PP_ADD_UNIFORM
};

static constexpr string_view ShaderNames[] = {
	#define PP_ADD_SHADER_NAME(name, ...) #name,
	DEMO_SHADERS(PP_ADD_SHADER_NAME)
	#undef PP_ADD_SHADER_NAME
};

////////////////////////////////////////////////////////////////

static constexpr string_view VectorComponents = "xyzwrgbastpq"sv;

constexpr bool IsMaybeSwizzle(string_view s) {
	if (s.empty())
		return false;
	for (char c : s)
		if (VectorComponents.find(c) == VectorComponents.npos)
			return false;
	return true;
}

////////////////////////////////////////////////////////////////

struct NameGenerator {
	std::vector<char> name;

	static constexpr char NextChar(char c, bool allow_digit) {
		if (c == '9')
			return 'A';
		if (c == 'Z')
			return 0;
		if (c == 'z')
			return allow_digit ? '0' : 'A';
		return c + 1;
	}

	NameGenerator() {
		name.reserve(256);
		name.assign({'b', '\0'});
	}

	string_view Next() {
		string_view result;

		do {
			bool carry = true;
			for (int i = name.size() - 2; i >= 0; --i) {
				char next = NextChar(name[i], i != 0);
				carry = (next == 0);
				if (carry)
					next = 'a';
				name[i] = next;
				if (!carry)
					break;
			}
			if (carry)
				name.insert(name.begin(), 'a');
			result = {name.data(), name.size() - 1};
		} while (IsMaybeSwizzle(result));

		return result;
	}
};

////////////////////////////////////////////////////////////////

void Tokenize(string_view code, Lexer& lexer, std::vector<Lexer::Token>& tokens) {
	printf("Generating token stream...");

	if (tokens.empty())
		tokens.reserve(code.size() / 2);

	lexer.SetSource(code);
	while (!lexer.IsDone()) {
		tokens.push_back(lexer.Peek());
		lexer.Consume();
	}

	printf(" %zd tokens\n", tokens.size());
}

////////////////////////////////////////////////////////////////

void RenameVectorFields(std::vector<Lexer::Token>& tokens, AtomList& atoms) {
	printf("Renaming vector fields...\n");

	std::vector<char> buffer;
	buffer.reserve(4096);

	for (size_t i = 1; i < tokens.size(); ++i) {
		auto& token = tokens[i];
		if (tokens[i - 1].type != Token::Type::Member || token.type != Token::Type::Identifier || !IsMaybeSwizzle(token.value))
			continue;

		string_view name = token.value;
		buffer.assign(name.begin(), name.end());

		for (char& c : buffer) {
			auto index = VectorComponents.find(c);
			assert(index != VectorComponents.npos);
			if (index == VectorComponents.npos)
				continue;
			c = VectorComponents[index & 3];
		}

		token.value = atoms.Intern({buffer.data(), buffer.size()});
	}
}

////////////////////////////////////////////////////////////////

enum class Preserve {
	Inputs,
	InputsAndOtputs,
};

void RenameIdentifiers(std::vector<Lexer::Token>& tokens, AtomList& atoms, Preserve mode = Preserve::InputsAndOtputs) {
	printf("Renaming identifiers...\n");

	/* map shader names to integer ids */
	std::unordered_map<string_view, int> entry_points;
	for (auto& name : ShaderNames)
		entry_points[name] = &name - ShaderNames;

	/* mark untouchable identifiers */
	std::unordered_set<string_view> untouchable;
	untouchable.reserve(size(Keywords) + size(Uniforms));
	for (auto keyword : Keywords)
		untouchable.insert(keyword);
	for (auto uniform : Uniforms)
		untouchable.insert(uniform);

	/* find inputs/outputs */
	for (size_t i = 0; i < tokens.size(); ++i) {
		Token& token = tokens[i];
		if (token.type != Token::Type::Identifier)
			continue;

		string_view value = token.value;
		if (value != "in"sv) {
			if (mode != Preserve::InputsAndOtputs || value != "out"sv)
				continue;
		}

		// in|out must be either the very first token, or preceded by a semicolon
		if (i != 0 && tokens[i - 1].type != Token::Type::Semicolon)
			continue;

		if (i + 2 >= tokens.size())
			continue;

		i += 2;
		while (i + 1 < tokens.size() && tokens[i].type != Token::Type::Semicolon) {
			untouchable.insert(tokens[i++].value);
			if (tokens[i].type == Token::Type::Comma)
				++i;
		}
	}

	/* find existing macros */
	std::unordered_set<string_view> existing_macros;
	for (size_t i = 0; i + 2 < tokens.size(); ++i) {
		Token& token = tokens[i];
		if (tokens[i+0].type != Token::Type::Directive ||
			tokens[i+1].type != Token::Type::Identifier ||
			tokens[i+2].type != Token::Type::Identifier ||
			tokens[i+1].value != "define"sv)
			continue;

		existing_macros.insert(tokens[i + 2].value);
		i += 3;
		while (i < tokens.size() && tokens[i].type != Token::Type::EndOfLine)
			++i;
	}

	/* sort identifiers by number of occurrences */
	std::unordered_map<const char*, size_t> ident_usage;
	ident_usage.reserve(tokens.size());
	for (Token& token : tokens)
		if (token.type == Token::Type::Identifier)
			ident_usage[token.value]++;

	struct Identifier {
		const char* name;
		size_t usage;

		bool operator<(const Identifier& rhs) const { return usage > rhs.usage; } // sort by usage, in descending order
	};

	std::vector<Identifier> identifiers;
	identifiers.reserve(ident_usage.size());
	for (auto& v : ident_usage)
		identifiers.push_back({v.first, v.second});
	std::sort(identifiers.begin(), identifiers.end());

	/* generate new identifier names */
	NameGenerator name_generator;
	std::unordered_map<string_view, const char*> rename;
	rename.reserve(identifiers.size());
	std::vector<string_view> new_macros;
	new_macros.reserve(identifiers.size());

	for (auto& id : identifiers) {
		string_view name = id.name;
		auto& new_name = rename[name];
		new_name = name.data();

		if (name == "define"sv)
			continue;

		if (IsMaybeSwizzle(name) || untouchable.find(name) != untouchable.end()) {
			int new_length = name_generator.name.size() - 1;
			int savings = id.usage * (name.size() - new_length);
			bool is_already_macro = existing_macros.find(name) != existing_macros.end();
			if (!is_already_macro)
				savings -= "\n#define  \n"sv.length() + new_length + name.size();
			if (savings > 4) {
				if (!is_already_macro)
					new_macros.push_back(name);
				new_name = atoms.Intern(name_generator.Next());
			}
		} else {
			auto entry_iter = entry_points.find(name);
			if (entry_iter != entry_points.end()) {
				char buf[64];
				sprintf(buf, "_%d", entry_iter->second);
				new_name = atoms.Intern(buf);
			} else {
				new_name = atoms.Intern(name_generator.Next());
			}
		}
	}

	/* add header with macros for untouchable identifiers */
	const size_t NumMacroDefTokens = 5;
	size_t num_macro_tokens = new_macros.size() * NumMacroDefTokens;
	tokens.resize(tokens.size() + num_macro_tokens);
	std::rotate(tokens.begin(), tokens.end() - num_macro_tokens, tokens.end());

	for (size_t i = 0; i < new_macros.size(); ++i) {
		auto& macro = new_macros[i];
		Token* token = &tokens[i * NumMacroDefTokens];
		token[0] = {Token::Type::Directive};
		token[1] = {Token::Type::Identifier, atoms.Intern("define")};
		token[2] = {Token::Type::Identifier, rename[macro]};
		token[3] = {Token::Type::Identifier, macro.data()};
		token[4] = {Token::Type::EndOfLine};
	}

	/* perform the actual token renaming */
	for (size_t i = num_macro_tokens; i < tokens.size(); ++i) {
		Token& token = tokens[i];
		if (token.type != Token::Type::Identifier)
			continue;
		const char* new_name = rename[token.value];
		if (new_name)
			token.value = new_name;
	}
}

////////////////////////////////////////////////////////////////

void GenerateCode(const std::vector<Lexer::Token>& tokens, std::vector<char>& out) {
	printf("Generating code...");

	out.reserve(tokens.size() * 2);

	Token::Type last_type = Token::Type::Invalid;
	for (auto& token : tokens) {
		if (token.type == Token::Type::Directive && last_type != Token::Type::EndOfLine && last_type != Token::Type::Invalid)
			out.push_back('\n');

		// constants and identifiers need whitespace between them, otherwise they can end up merged
		if (token.type == Token::Type::Identifier || token.type == Token::Type::Constant) {
			if (last_type == Token::Type::Identifier || last_type == Token::Type::Constant)
				out.push_back(' ');
		}

		auto s = token.ToString();
		out.insert(out.end(), s.begin(), s.end());
		last_type = token.type;
	}

	printf(" %zd chars\n", out.size());

	out.push_back(0);
}

////////////////////////////////////////////////////////////////

void Print(FILE* out, string_view code) {
	while (!code.empty()) {
		auto eol = code.find('\n');
		string_view line = code.substr(0, eol);
		if (eol == code.npos)
			code = {};
		else
			code.remove_prefix(eol + 1);

		const size_t MaxLineLength = 8192;
		while (!line.empty()) {
			size_t length = std::min(line.size(), MaxLineLength);
			bool last_part = length == line.size();
			bool add_newline = last_part && !code.empty();
			const char* format = add_newline ? "\"%.*s\\n\"\n" : "\"%.*s\"\n";
			fprintf(out, format, int(length), line.data());
			line.remove_prefix(length);
		}
	}
}

////////////////////////////////////////////////////////////////

int main(int argc, const char** argv) {
	Options options;
	if (!GatherOptions(options, argc, argv))
		return 1;

	std::vector<char> source_code;
	std::vector<Lexer::Token> tokens;
	std::vector<char> generated_code;
	Lexer lexer;

	FILE* out = fopen(options.output_path.c_str(), "w");
	if (!out) {
		printf("ERROR: Could not open output file %s\n", options.output_path.c_str());
		return 1;
	}
	auto close_output = scope_exit { fclose(out); out = nullptr; };

	/* write header */
	fprintf(out,
		"#pragma once\n"
		"// auto-generated, do not modify\n"
	);

	/* list all .glsl files */
	std::error_code file_list_error = {};
	fs::directory_iterator file_list{options.source_path, file_list_error};
	if (file_list_error) {
		printf("ERROR: Could not list files in %s\n", options.source_path.c_str());
		return 1;
	}

	for (auto& item : file_list) {
		auto path = item.path();
		if (path.extension() != L".glsl"sv)
			continue;

		using clock = std::chrono::high_resolution_clock;
		auto time_begin = clock::now();

		auto full_path = path.generic_string();
		auto file_name = path.filename().string();
		auto file_name_no_extension = path.filename().replace_extension().string();

		Preserve mode = file_name_no_extension.find("vertex") != std::string::npos ? Preserve::InputsAndOtputs : Preserve::Inputs;

		printf("--- Compiling %s ---\n", file_name.c_str());

		printf("Reading file...");
		if (!ReadFile(path.string().c_str(), source_code)) {
			return 1;
		}
		printf(" %zd bytes\n", source_code.size());

		tokens.clear();
		Tokenize({source_code.data(), source_code.size()}, lexer, tokens);

		RenameVectorFields(tokens, lexer.GetAtoms());
		RenameIdentifiers(tokens, lexer.GetAtoms(), mode);

		generated_code.clear();
		GenerateCode(tokens, generated_code);

		/* file header */
		fprintf(out,
			"\n"
			"// %s: %zd => %zd (%.1f%%)\n"
			"static constexpr char g_%s[] =\n",
			full_path.c_str(), source_code.size(), generated_code.size(), (float)generated_code.size() / (float)source_code.size() * 100.f,
			file_name_no_extension.c_str()
		);

		Print(out, {generated_code.data(), generated_code.size()});

		fprintf(out, ";\n");

		auto msec_elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(clock::now() - time_begin);
		printf("%lld msec elapsed\n\n", msec_elapsed.count());
	}

	return 0;
}
