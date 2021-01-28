#include "shader_compiler.h"

////////////////////////////////////////////////////////////////

void Print(const char* format, ...) {
	va_list args;
	va_start(args, format);
	vprintf_s(format, args);
	va_end(args);
}

////////////////////////////////////////////////////////////////

struct Options {
	std::string output_path;
	std::string source_path;
};

bool GatherOptions(Options& options, int argc, const char** argv) {
	if (argc == 1) {
		options.source_path = "../../../data/shaders";
		options.output_path = "../../demo/cooked/cooked_shaders.h";

		Print(
			"No arguments specified, using default paths:\n"
			"  source_folder: %s\n"
			"  output_file:   %s\n"
			"\n",
			options.source_path.c_str(),
			options.output_path.c_str()
		);
	} else if (argc < 3) {
		Print("Usage: %s source_folder output_file\n\n",
			fs::path(argv[0]).filename().string().c_str());
		return false;
	} else {
		options.source_path = argv[1];
		options.output_path = argv[2];
	}

	return true;
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
	VaryingMap varyings;
	std::unordered_map<string_view, const char*> translation;
	SectionList sections;
	ShaderDependencies shader_deps;
	std::vector<char> generated_code;
	std::vector<std::string> modules;
	Lexer lexer;

	FILE* out = fopen(options.output_path.c_str(), "w");
	if (!out) {
		Print("ERROR: Could not open output file %s\n", options.output_path.c_str());
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

	for (size_t stage = 0; stage < ShaderStage::Count; ++stage) {
		std::string stage_name             = std::string(ShaderStage::Names[stage]);
		std::string file_name_no_extension = stage_name + "_shaders";
		std::string file_name              = file_name_no_extension + ".glsl";
		std::string full_path              = options.source_path + "/" + file_name;
		ShaderStage::Type stage_type       = ShaderStage::Type(stage);

		using clock = std::chrono::high_resolution_clock;
		auto time_begin = clock::now();

		Print("--- Compiling %s ---\n", file_name.c_str());

		Print("Reading file...");
		if (!ReadFile(full_path.c_str(), source_code)) {
			return 1;
		}
		Print(" %zd bytes\n", source_code.size());

		if (!Tokenize({source_code.data(), source_code.size()}, lexer, tokens) ||
			!RenameVectorFields(tokens, lexer.GetAtoms()) ||
			!RenameIdentifiers(stage_type, tokens, lexer.GetAtoms(), varyings, translation) ||
			!SplitIntoSections(tokens, sections) ||
			!Link(tokens, sections, shader_deps) ||
			!GenerateCode(tokens, sections, generated_code)) {
			return 1;
		}

		// Same as 'clean_path', but without the (potentially repeated) "../" prefix
		// that gets added when running the compiler from within MSVC.
		// Note: still null-terminated.
		string_view clean_full_path = {full_path.data(), full_path.size()};
		while (RemovePrefix(clean_full_path, "../"sv))
			;

		/* file header */
		fprintf(out,
			"\n"
			"// %s: %zd => %zd (%.1f%%)\n"
			"namespace cooked::%s {\n"
			"static constexpr char code[] =\n",
			clean_full_path.data(), source_code.size(), generated_code.size(), (float)generated_code.size() / (float)source_code.size() * 100.f,
			file_name_no_extension.c_str()
		);

		PrintCode(out, {generated_code.data(), generated_code.size()}, sections);
		if (OutputTranslationMap)
			PrintTranslationMap(out, translation);
		PrintSections(out, {generated_code.data(), generated_code.size()}, sections, shader_deps);

		/* footer */
		fprintf(out, "} // namespace cooked::%s\n", file_name_no_extension.c_str());

		auto msec_elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(clock::now() - time_begin);
		Print("%lld msec elapsed\n\n", msec_elapsed.count());

		modules.push_back("cooked::" + file_name_no_extension);
	}

	PrintModules(out, modules);

	return 0;
}
