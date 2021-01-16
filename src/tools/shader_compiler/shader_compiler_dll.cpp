#include "shader_compiler.h"
#include "shader_compiler_interface.h"

#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#define NOMINMAX
#define _WIN32_WINNT 0x0603
#include <Windows.h>

////////////////////////////////////////////////////////////////

void Print(const char* format, ...) {
	/* empty to avoid console spam during recompilation */
}

////////////////////////////////////////////////////////////////

class ShaderCompiler : public IShaderCompiler {
public:
	void							Destroy() override { delete this; }
	const Module*					Compile(const char* source_path) override;

	struct StageData {
		std::vector<char>			source_code;
		std::vector<Lexer::Token>	tokens;
		std::unordered_map<string_view, const char*> translation;
		SectionList					sections;
		ShaderDependencies			shader_deps;
		std::vector<char>			generated_code;
		std::vector<u32>			section_sizes;

	}								stage_data[ShaderStage::Count];
	Module							modules[ShaderStage::Count];
	Lexer							lexer;
};

const IShaderCompiler::Module* ShaderCompiler::Compile(const char* source_path) {
	memset(&modules, 0, sizeof(&modules));

	try {
		for (size_t stage_index = 0; stage_index < ShaderStage::Count; ++stage_index) {
			std::string stage_name             = std::string(ShaderStage::Names[stage_index]);
			std::string file_name_no_extension = stage_name + "_shaders";
			std::string file_name              = file_name_no_extension + ".glsl";
			std::string full_path              = std::string(source_path) + "/" + file_name;
			StageData& stage                   = stage_data[stage_index];

			if (!ReadFile(full_path.c_str(), stage.source_code))
				return nullptr;

			Preserve mode = stage_index == ShaderStage::Vertex ? Preserve::InputsAndOtputs : Preserve::Inputs;

			if (!Tokenize({stage.source_code.data(), stage.source_code.size()}, lexer, stage.tokens) ||
				!RenameVectorFields(stage.tokens, lexer.GetAtoms()) ||
				!RenameIdentifiers(stage.tokens, lexer.GetAtoms(), mode, stage.translation) ||
				!SplitIntoSections(stage.tokens, stage.sections) ||
				!Link(stage.tokens, stage.sections, stage.shader_deps) ||
				!GenerateCode(stage.tokens, stage.sections, stage.generated_code)) {
				return nullptr;
			}

			stage.section_sizes.resize(stage.sections.size());
			for (size_t section_index = 0; section_index < stage.sections.size(); ++section_index)
				stage.section_sizes[section_index] = GetSectionSize(stage.sections, section_index, stage.generated_code);

			modules[stage_index].code = stage.generated_code.data();
			modules[stage_index].num_sections = stage.sections.size();
			modules[stage_index].section_sizes = stage.section_sizes.data();
			modules[stage_index].shader_deps = stage.shader_deps.data;
		}
	} catch (...) {
		return nullptr;
	}

	return modules;
}

////////////////////////////////////////////////////////////////

extern "C" __declspec(dllexport)
IShaderCompiler* __cdecl CreateShaderCompiler(unsigned version) {
	if (version != Demo::Shader::Version) {
		DebugPrint("Shader compiler version mismatch: expected 0x%08x, found 0x%08x.\n", Demo::Shader::Version, version);
		return nullptr;
	}
	return new ShaderCompiler;
}

////////////////////////////////////////////////////////////////

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpReserved) {
	return TRUE;
}
