#include "../common.h"
#include "lexer.h"
#include "../../demo/resource_def.h"
#include <filesystem>
#include <chrono>
#include <stdexcept>

using Token = Lexer::Token;
namespace fs = std::filesystem;

#define INDENT "   "

// Debug options ///////////////////////////////////////////////

const bool
	Verbose					= false,
	InsertSectionMarkers	= false,
	OutputTranslationMap	= false
;

////////////////////////////////////////////////////////////////

const size_t MaxLineLength = 124;

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

void Error(const char* format, ...) {
	char message[8192];
	va_list args;
	va_start(args, format);
	vsprintf_s(message, format, args);
	va_end(args);

	printf("ERROR: %s.\n", message);
	MessageBoxA(0, message, "Error", MB_OK | MB_ICONERROR);

	throw std::runtime_error(message);
}

////////////////////////////////////////////////////////////////

static constexpr string_view Keywords[] = {
	"void","float","int","uint","bool","vec2","ivec2","uvec2","bvec2","vec3","ivec3","uvec3","bvec3","vec4","ivec4","uvec4","bvec4",
	"struct","mat2","mat3","mat4","mat2x2","mat2x3","mat2x4","mat3x2","mat3x3","mat3x4","mat4x2","mat4x3","mat4x4","sampler2D",
	"sampler3D","samplerCube","sampler2DShadow","samplerCubeShadow","sampler2DArray","uniform","const","in","out","inout","layout",
	"location","main","if","else","for","while","do","switch","case","default","break","continue","return","discard",
	"texture","textureLod","textureProj","texelFetch","textureGrad","textureSize","floor","round","fract","mod","sin","asin","cos",
	"acos","tan","atan","radians","degrees","mix","smoothstep","step","abs","sign","max","min","clamp","reflect","cross",
	"normalize","length","sqrt","inversesqrt","pow","exp","exp2","log","log2","dot","fwidth","dFdx","dFdy","transpose",
	"any","all","not","equal","notEqual","lessThan","lessThanEqual","greaterThan","greaterThanEqual",
	"gl_Position","gl_FragCoord",
};

static constexpr string_view UniformNames[] = {
	#define PP_ADD_UNIFORM(name, ...) #name##sv,
	DEMO_UNIFORMS(PP_ADD_UNIFORM)
	#undef PP_ADD_UNIFORM
};

static constexpr string_view ShaderNames[] = {
	#define PP_ADD_SHADER_NAME(name, ...) #name,
	DEMO_SHADERS(PP_ADD_SHADER_NAME)
	#undef PP_ADD_SHADER_NAME
};

static constexpr string_view ShaderStages[] = {
	"vertex",
	"fragment",
};

////////////////////////////////////////////////////////////////

static constexpr string_view VectorComponents = "rgbaxyzwstpq"sv;

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
		const bool AllowUppercase = false;
		if (c == '9') return AllowUppercase ? 'A' : 0;
		if (c == 'Z') return 0;
		if (c == 'z') return allow_digit ? '0' : AllowUppercase ? 'A' : 0;
		return c + 1;
	}

	NameGenerator() {
		name.reserve(256);
		name.assign({'b', '\0'});
	}

	string_view Next(Function<bool(string_view)> accept = [](string_view){return true;}) {
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
		} while (IsMaybeSwizzle(result) || !accept(result));

		return result;
	}
};

////////////////////////////////////////////////////////////////

void Tokenize(string_view code, Lexer& lexer, std::vector<Lexer::Token>& tokens) {
	printf("Generating token stream...");

	tokens.clear();
	tokens.reserve(code.size() / 2);

	lexer.SetSource(code);
	while (!lexer.IsDone()) {
		tokens.push_back(lexer.Peek());
		lexer.Consume();
	}

	printf(" %zd tokens\n", tokens.size());
}

////////////////////////////////////////////////////////////////

bool SkipPragma(const std::vector<Token>& tokens, size_t& i) {
	if (tokens[i].type != '#' ||
		i == tokens.size() - 1 ||
		tokens[i + 1].type != Token::Type::Identifier ||
		tokens[i + 1].value != "pragma"sv)
		return false;

	i += 2; // skip past "pragma"

	while (i < tokens.size() && tokens[i].type != Token::Type::EndOfLine)
		++i;

	return true;
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

void RenameIdentifiers(std::vector<Lexer::Token>& tokens, AtomList& atoms, Preserve mode, std::unordered_map<string_view, const char*>& rename) {
	printf("Renaming identifiers...\n");

	/* map shader names to integer ids */
	std::unordered_map<string_view, int> entry_points;
	for (auto& name : ShaderNames)
		entry_points[name] = &name - ShaderNames;

	/* map uniform names to integer ids */
	std::unordered_map<string_view, int> uniforms;
	for (auto& name : UniformNames)
		uniforms[name] = &name - UniformNames;

	/* mark untouchable identifiers */
	std::unordered_set<string_view> untouchable;
	untouchable.reserve(size(Keywords));
	for (auto keyword : Keywords)
		untouchable.insert(keyword);

	/* find inputs/outputs */
	for (size_t i = 0; i < tokens.size(); ++i) {
		Token& token = tokens[i];
		if (SkipPragma(tokens, i))
			continue;

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
		if (tokens[i+0].type != '#' ||
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
	for (size_t i = 0; i < tokens.size(); ++i) {
		if (SkipPragma(tokens, i))
			continue;

		Token& token = tokens[i];
		if (token.type == Token::Type::Identifier)
			ident_usage[token.value]++;
	}

	struct Identifier {
		const char* name;
		size_t usage;

		bool operator<(const Identifier& rhs) const { return usage > rhs.usage; } // sort by usage, in descending order
	};

	std::vector<Identifier> identifiers;
	identifiers.reserve(ident_usage.size());
	for (auto& v : ident_usage)
		identifiers.push_back({v.first, v.second});
	std::stable_sort(identifiers.begin(), identifiers.end());

	/* generate new identifier names */
	NameGenerator name_generator;
	rename.clear();
	rename.reserve(identifiers.size());
	std::vector<string_view> new_macros;
	new_macros.reserve(identifiers.size());

	auto next_name = [&] {
		return name_generator.Next([&] (string_view name) {
			return untouchable.find(name) == untouchable.end();
		});
	};

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
				new_name = atoms.Intern(next_name());
			}
		} else {
			auto entry_iter = entry_points.find(name);
			if (entry_iter != entry_points.end()) {
				char buf[64];
				sprintf(buf, "_%d", entry_iter->second);
				new_name = atoms.Intern(buf);
				continue;
			}

			auto uniform_iter = uniforms.find(name);
			if (uniform_iter != uniforms.end()) {
				char buf[64];
				sprintf(buf, "U%d", uniform_iter->second);
				new_name = atoms.Intern(buf);
				continue;
			}

			new_name = atoms.Intern(next_name());
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
		if (SkipPragma(tokens, i))
			continue;

		Token& token = tokens[i];
		if (token.type != Token::Type::Identifier)
			continue;
		const char* new_name = rename[token.value];
		if (new_name)
			token.value = new_name;
	}
}

////////////////////////////////////////////////////////////////

struct Section {
	std::string		name;
	u32				dependencies = 0;
	u32				offset = 0;
};

using SectionList = std::vector<Section>;

template <typename Data>
size_t GetSectionEnd(const SectionList& sections, size_t index, const Data& data) {
	return index + 1 < sections.size() ? sections[index + 1].offset : data.size();
}

void SplitIntoSections(std::vector<Lexer::Token>& tokens, SectionList& sections) {
	sections.clear();

	/* add global section (always present) */
	Section& core = sections.emplace_back();
	core.name = "@global";

	for (size_t i = 0; i + 2 < tokens.size(); ++i) {
		if (tokens[i + 0].type != '#' ||
			tokens[i + 1].type != Token::Type::Identifier || tokens[i + 1].value != "pragma"sv ||
			tokens[i + 2].type != Token::Type::Identifier || tokens[i + 2].value != "section"sv)
			continue;

		Section& section = sections.emplace_back();
		section.offset = i;
		section.dependencies = 1 << 0; // add dependency on global section

		i += 3; // skip past "section"

		/* determine section name */
		if (i < tokens.size() && tokens[i].type == Token::Type::Identifier) {
			section.name = tokens[i].value;
			++i;
		} else {
			char generated_name[32];
			sprintf(generated_name, "@section%zd", sections.size() - 1);
			section.name = generated_name;
		}

		/* skip colon */
		if (i + 1 < tokens.size() && tokens[i].type == ':')
			++i;

		/* read dependencies */
		while (i < tokens.size()) {
			if (tokens[i].type == Token::Type::EndOfLine) {
				++i;
				break;
			}

			if (tokens[i].type == ',') {
				++i;
				continue;
			}

			if (tokens[i].type == Token::Type::Identifier) {
				ptrdiff_t dependency = -1;

				for (size_t j = 1; j < sections.size() - 1; ++j) {
					if (sections[j].name == tokens[i].value) {
						dependency = j;
						break;
					}
				}

				if (dependency == -1)
					Error("Section '%s' (required by '%s') not found", tokens[i].value, section.name.c_str());

				section.dependencies |= (1 << dependency) | sections[dependency].dependencies;

				++i;
				continue;
			}
		}

		/* remove pragma directive from token stream */
		tokens.erase(tokens.begin() + section.offset, tokens.begin() + i);
	}
}

////////////////////////////////////////////////////////////////

struct ShaderDependencies {
	i32 data[std::size(ShaderNames)];

	static constexpr size_t		size()		{ return std::size(ShaderNames); }
	void						clear()		{ memset(&data, 0, sizeof(data)); }
};

void Link(const std::vector<Lexer::Token>& tokens, const SectionList& sections, ShaderDependencies& shader_deps) {
	shader_deps.clear();

	size_t section_index = 0;
	for (size_t token_index = 0; token_index < tokens.size(); ++token_index) {
		if (section_index < sections.size() - 1 && token_index == sections[section_index + 1].offset)
			++section_index;

		const Token& token = tokens[token_index];
		if (token.type != Token::Type::Identifier || token.value[0] != '_')
			continue;

		int shader_index = -1;
		sscanf(token.value + 1, "%d", &shader_index);

		assert(shader_index >= 0);
		assert(shader_index < std::size(ShaderNames));

		if (shader_deps.data[shader_index] == 0) {
			shader_deps.data[shader_index] = sections[section_index].dependencies | (1 << section_index);

			if (Verbose)
				printf("Found %s in section %s\n", ShaderNames[shader_index].data(), sections[section_index].name.c_str());
		}
	}
}

////////////////////////////////////////////////////////////////

void GenerateCode(const std::vector<Lexer::Token>& tokens, SectionList& sections, std::vector<char>& out) {
	printf("Generating code...");

	out.clear();
	out.reserve(tokens.size() * 2);

	Token::Type last_type = Token::Type::Invalid;

	/* generate code for each section */
	for (size_t section_index = 0; section_index < sections.size(); ++section_index) {
		Section& section = sections[section_index];
		size_t num_tokens = GetSectionEnd(sections, section_index, tokens) - section.offset;
		size_t code_start = out.size();

		auto append_string = [&] (string_view str) {
			out.insert(out.end(), str.begin(), str.end());
		};

		if (InsertSectionMarkers) {
			append_string("/*");
			append_string(section.name);
			append_string("*/\n");
		}

		for (size_t token_index = section.offset, end_token_index = section.offset + num_tokens; token_index < end_token_index; ++token_index) {
			const Token& token = tokens[token_index];
			if (token.type == Token::Type::Directive && last_type != Token::Type::EndOfLine && last_type != Token::Type::Invalid)
				out.push_back('\n');

			/* prevent constants and identifiers from being merged by inserting a space between them */
			if (token.type == Token::Type::Identifier || token.type == Token::Type::Constant) {
				if (last_type == Token::Type::Identifier || last_type == Token::Type::Constant)
					out.push_back(' ');
			}

			append_string(token.ToString());
			last_type = token.type;
		}

		/* mark section offset in generated code */
		section.offset = code_start;
	}

	printf(" %zd chars\n", out.size());
}

////////////////////////////////////////////////////////////////

void PrintCode(FILE* out, string_view code, const SectionList& sections) {
	for (size_t section_index = 0; section_index < sections.size(); ++section_index) {
		const Section& section = sections[section_index];

		size_t section_end = GetSectionEnd(sections, section_index, code);
		size_t section_size = section_end - section.offset;
		string_view section_code = code.substr(section.offset, section_size);

		fprintf(out, "/* ---- %s: %zd chars (%.1f%%) ---- */\n",
			section.name.c_str(), section_size, 100.f * section_size / code.size());

		while (!section_code.empty()) {
			string_view line = section_code.substr(0, MaxLineLength - 2); // exclude start/end quotation marks
			auto eol = line.find('\n');
			if (eol != string_view::npos) {
				section_code.remove_prefix(eol + 1);
				fprintf(out, "\"%.*s\\n\"\n", int(eol), line.data());
			} else {
				section_code.remove_prefix(line.size());
				bool last = section_code.empty() && section_index == sections.size() - 1;
				fprintf(out, "\"%.*s\"%s\n", int(line.size()), line.data(), last ? ";" : "");
			}
		}
	}
}

////////////////////////////////////////////////////////////////

void PrintTranslationMap(FILE* out, const std::unordered_map<string_view, const char*>& translation) {
	fprintf(out,
		"/*\n"
		"Identifier map:\n"
		"----------------\n"
	);

	for (const auto& entry : translation)
		if (entry.second)
			fprintf(out, "%.*s = %s\n", int(entry.first.size()), entry.first.data(), entry.second);

	fprintf(out, "*/\n");
}

////////////////////////////////////////////////////////////////

void PrintSections(FILE* out, string_view generated_code, const SectionList& sections, const ShaderDependencies& shader_deps) {
	ArrayPrinter print(out, MaxLineLength);

	print << "static constexpr u32 section_sizes[] = {"sv;
	for (size_t section_index = 0; section_index < sections.size(); ++section_index) {
		const Section& section = sections[section_index];
		size_t section_size = GetSectionEnd(sections, section_index, generated_code) - section.offset;
		print << "/*"sv << section.name << "*/"sv << i32(section_size) << ","sv;
	}
	print << "};"sv;
	print.Flush();

	print << "static constexpr u32 shader_deps[] = {"sv;
	for (i32 shader_dependency_mask : shader_deps.data) {
		print << shader_dependency_mask << ","sv;
	}
	print << "};"sv;
	print.Flush();
}

////////////////////////////////////////////////////////////////

void PrintModules(FILE* out, const std::vector<std::string>& module_names) {
	fprintf(out,
		"\n"
		"static constexpr Gfx::Shader::Module shader_modules[] = {\n"
		"#ifdef DISABLE_SHADER_STITCHING\n"
	);
	for (const std::string& module : module_names) {
		fprintf(out, "\t{ %s::code },\n", module.c_str());
	}
	fprintf(out, "#else\n");
	for (const std::string& module : module_names) {
		fprintf(out, "\t{ size(%s::section_sizes), %s::code, %s::section_sizes, %s::shader_deps },\n",
			module.c_str(), module.c_str(), module.c_str(), module.c_str()
		);
	}
	fprintf(out,
		"#endif\n"
		"};\n"
	);
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

int main(int argc, const char** argv) {
	Options options;
	if (!GatherOptions(options, argc, argv))
		return 1;

	std::vector<char> source_code;
	std::vector<Lexer::Token> tokens;
	std::unordered_map<string_view, const char*> translation;
	SectionList sections;
	ShaderDependencies shader_deps;
	std::vector<char> generated_code;
	std::vector<std::string> modules;
	Lexer lexer;

	FILE* out = fopen(options.output_path.c_str(), "w");
	if (!out) {
		printf("ERROR: Could not open output file %s\n", options.output_path.c_str());
		return 1;
	}
	auto close_output = scope_exit { fclose(out); out = nullptr; };

	/* write header/version check */
	fprintf(out,
		"#pragma once\n"
		"\n"
		"// auto-generated, do not modify\n"
		"static_assert(0x%08xU == Demo::Shader::Version, \"Shader definition mismatch, please recompile the shader compiler & shaders\");\n",
		Demo::Shader::Version
	);

	for (std::string_view stage : ShaderStages) {
		std::string file_name_no_extension = std::string(stage) + "_shaders";
		std::string file_name              = file_name_no_extension + ".glsl";
		std::string full_path              = options.source_path + "/" + file_name;

		using clock = std::chrono::high_resolution_clock;
		auto time_begin = clock::now();

		Preserve mode = file_name_no_extension.find("vertex") != std::string::npos ? Preserve::InputsAndOtputs : Preserve::Inputs;

		printf("--- Compiling %s ---\n", file_name.c_str());

		printf("Reading file...");
		if (!ReadFile(full_path.c_str(), source_code)) {
			return 1;
		}
		printf(" %zd bytes\n", source_code.size());

		Tokenize({source_code.data(), source_code.size()}, lexer, tokens);
		RenameVectorFields(tokens, lexer.GetAtoms());
		RenameIdentifiers(tokens, lexer.GetAtoms(), mode, translation);
		SplitIntoSections(tokens, sections);
		Link(tokens, sections, shader_deps);
		GenerateCode(tokens, sections, generated_code);

		/* file header */
		fprintf(out,
			"\n"
			"// %s: %zd => %zd (%.1f%%)\n"
			"namespace cooked::%s {\n"
			"static constexpr char code[] =\n",
			full_path.c_str(), source_code.size(), generated_code.size(), (float)generated_code.size() / (float)source_code.size() * 100.f,
			file_name_no_extension.c_str()
		);

		PrintCode(out, {generated_code.data(), generated_code.size()}, sections);
		if (OutputTranslationMap)
			PrintTranslationMap(out, translation);
		PrintSections(out, {generated_code.data(), generated_code.size()}, sections, shader_deps);

		/* footer */
		fprintf(out, "} // namespace cooked::%s\n", file_name_no_extension.c_str());

		auto msec_elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(clock::now() - time_begin);
		printf("%lld msec elapsed\n\n", msec_elapsed.count());

		modules.push_back("cooked::" + file_name_no_extension);
	}

	PrintModules(out, modules);

	return 0;
}
