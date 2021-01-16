#pragma once

class IShaderCompiler {
public:
	using CreateFunction = IShaderCompiler* (__cdecl *)(unsigned);

	struct Module {
		u32				num_sections;
		const char*		code;
		const u32*		section_sizes;
		const u32*		shader_deps;
	};

	virtual void			Destroy() = 0;
	virtual const Module*	Compile(const char* source_path) = 0;
};
