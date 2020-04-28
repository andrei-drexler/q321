////////////////////////////////////////////////////////////////
var options = {
	trim_newlines		: true,
	consistent_floats	: false,
	rename_ids			: true,
	rename_vec_fields	: true,
	rename_globals		: true,
	min_macro_savings	: 8
};
////////////////////////////////////////////////////////////////

var debugMissingSymbol;// = "bS";
var startTime = new Date();

function repeat(str, count) {
	var result = "";
	if (count <= 0)
		return result;
	for (var mask = 1; mask <= count; mask <<= 1, str += str)
		if (count & mask)
			result += str;
	return result;
}

function padRight(str, count) {
	return str + repeat(" ", count - str.length);
}

function time(name, code) {
	var t0 = new Date();
	var result = code();
	var t1 = new Date();
	var msec = (t1 - t0).valueOf();
	if ("name" in timings)
		timings[name] += msec;
	else
		timings[name] = msec;
	return result;
}

////////////////////////////////////////////////////////////////

var timings = {};
var srcPaths = [];
var dstPath;

for (var i = 0; i < WScript.Arguments.length; ++i) {
	var arg = WScript.Arguments(i);
	if (arg == "-o") {
		if (i == WScript.Arguments.length - 1) {
			WScript.Echo("Error: must supply output file name after '-o' argument");
			WScript.Quit(1);
		}
		dstPath = WScript.Arguments(++i);
		continue;
	}
	srcPaths.push(arg);
}

var fso = new ActiveXObject("Scripting.FileSystemObject");
var ForReading = 1, ForWriting = 2, ForAppending = 8;
var ASCII = 0;

var output = "";
output += "#pragma once\n";
output += "// auto-generated, do not modify (see " + WScript.ScriptName + ")\n";

output += "\n";
for (var opt in options) {
	var padded  = padRight(opt, 20);
	if (dstPath)
		WScript.Echo(padded + ": " + options[opt]);
	output += "// " + padded + ": " + options[opt] + "\n";
}
if (dstPath)
	WScript.Echo();

for (var srcIndex in srcPaths) {
	var infile = fso.OpenTextFile(srcPaths[srcIndex]);
	var source = infile.ReadAll();
	infile.Close();
	
	var initialSize = source.length;
	var originalSource = source;
	
	var protectDirectives = [];
	if (options.rename_ids) {
		var protectExp = /\/\/[ \t]*\$[ \t]*protect(?:\(([^)]*)\))? [ \t]*([^\r\n]+)[\r\n]/g, match;
		while ((match = protectExp.exec(source)) !== null) {
			protectDirectives.push(RegExp(match[2], match[1] || 'gm'));
		}
	}

	source = source.replace(/\r/g, "");						// normalize newline sequences
	source = source.replace(/\\\n/g, " ");					// merge continued lines (e.g. multi-line macros)
	source = source.replace(/\/\/.*$/gm, " ");				// remove single-line (C++-style) comments
	source = source.replace(/\/\*.*?\*\//gm, " ");			// remove multi-line (C-style) comments
	source = source.replace(/[ \t]+/g, " ");				// merge whitespace
	source = source.replace(/([ \t]*\n[ \t]*)+/gm, "\n");	// merge newlines

	// the only spaces we must keep are the ones between tokens
	// that would otherwise be merged, e.g. 123 456, or abc 123
	source = source.replace(/([^a-z0-9]) /gi, "$1");
	source = source.replace(/ ([^a-z0-9])/gi, "$1");
	
	if (options.consistent_floats) {
		source = source.replace(/([^0-9])(\.[0-9])/g, "$10$2");			// .123 -> 0.123
		source = source.replace(/([0-9]\.)([^_a-z0-9])/g, "$10$2");		// 123. -> 123.0
	}

	if (options.trim_newlines) {
		function cleanUp(str) {
			// same with newlines, if removing a newline would result in a merge
			// we replace it with a space, otherwise we remove it completely
			str = str.replace(/([^.a-z0-9])\n([.a-z0-9])/gi, "$1$2");
			str = str.replace(/([.a-z0-9])\n([^.a-z0-9])/gi, "$1$2");
			str = str.replace(/([.a-z0-9])\n([.a-z0-9])/gi, "$1 $2");
			str = str.replace(/\n/g, "");
			if (str.length > 0)
				str += "\n";
			return str;
		}
		
		var tmp = "", cursor = 0;
		
		var ppExp = /^#[^#\n]+$/gm;
		while ((match = ppExp.exec(source)) !== null) {
			tmp += cleanUp(source.substr(cursor, match.index - cursor));
			tmp += match[0] + "\n"; // preprocessor directives require a newline
			cursor = ppExp.lastIndex;
		}
		tmp += cleanUp(source.substr(cursor, source.length - cursor));
		source = tmp;
	}
	
	if (options.rename_ids) {
		// incomplete!
		var builtinList = [
			'void','float','int','uint','bool','vec2','ivec2','uvec2','bvec2','vec3','ivec3','uvec3','bvec3','vec4','ivec4','uvec4','bvec4',
			'struct','mat2','mat3','mat4','mat2x2','mat2x3','mat2x4','mat3x2','mat3x3','mat3x4','mat4x2','mat4x3','mat4x4','sampler2D',
			'sampler3D','samplerCube','sampler2DShadow','samplerCubeShadow','sampler2DArray','uniform','const','in','out','inout','layout',
			'location','main','if','else','for','while','do','switch','case','default','break','continue','return','discard',
			'texture','textureLod','textureProj','texelFetch','textureGrad','textureSize','floor','round','fract','mod','sin','asin','cos',
			'acos','tan','atan','radians','degrees','mix','smoothstep','step','abs','sign','max','min','clamp','reflect','normalize','length','sqrt',
			'pow','exp','exp2','log','log2','dot','fwidth','dFdx','dFdy',
			'any','all','not','equal','notEqual','lessThan','lessThanEqual','greaterThan','greaterThanEqual',
			'gl_Position','gl_FragCoord',
		];
		
		isBuiltin = {};
		for (var i in builtinList) {
			isBuiltin[builtinList[i]] = true;
		}

		isProtected = {};
		for (var directiveIndex in protectDirectives) {
			var userExp = protectDirectives[directiveIndex], match;
			while ((match = userExp.exec(originalSource)) !== null) {
				var id = match[1] || match[0]; // if no capture group, use whole match
				isProtected[id] = true;
			}
		}

		isGlobal = {};
		// find in/out/uniforms
		var uniformExp = /(in|out|uniform) [a-z0-9]+ ([_a-z][_a-z0-9]*(?:,[_a-z][_a-z0-9]*)*);/gi, match;
		while ((match = uniformExp.exec(source)) !== null) {
			var parts = match[2].split(',');
			for (var partIndex in parts) {
				if (!options.rename_globals)
					isProtected[parts[partIndex]] = true;
				isGlobal[parts[partIndex]] = true;
			}
		}

		var fieldExp = /\b[rgbaxyzwstpq]+\b/;
		function isFieldMaybe(str) {
			return str.match(fieldExp);
		}
		
		if (options.rename_vec_fields) {
			var original = "rgbastpqxyzw";
			var replacement = {};
			for (var i = 0; i < original.length; ++i) {
				replacement[original.charAt(i)] = original.charAt(i % 4);
			}

			source = source.replace(/\.[rgbaxyzwstpq]+\b/g, function(match) {
				var result = ".";
				for (var i = 1; i < match.length; ++i)
					result += replacement[match.charAt(i)];
				return result;
			});
		}
		
		function needsMacro(str) {
			return isBuiltin[str] || isGlobal[str] || isFieldMaybe(str);
		}
		
		identUsage = {};
		identList = [];
		var idExp = /\b[_a-z][_a-z0-9]*\b/gi;
		while ((match = idExp.exec(source)) !== null) {
			// don't try to rename preprocessor directives
			if (match.index > 0 && source.charAt(match.index - 1) == '#')
				continue;

			if (match[0] in identUsage) {
				identUsage[match[0]]++;
			} else {
				identUsage[match[0]] = 1;
				identList.push(match[0]);
			}
		}

		identSavings = {};
		for (var ident in identUsage) {
			var savings = (ident.length - 1) * identUsage[ident];
			if (needsMacro(ident))
				savings -= "\n#define X \n".length + options.min_macro_savings;
			identSavings[ident] = savings;
		}
		//identList.sort(function(a, b){ return identSavings[b] - identSavings[a]; });
		identList.sort(function(a, b){ return identUsage[b] - identUsage[a]; });
		
		function nextChar(c, allowDigit) {
			if (c === '9')
				return 'A';
			if (c === 'Z')
				return '';
			if (c === 'z')
				return allowDigit ? '0' : 'A';
			return String.fromCharCode(c.charCodeAt(c.length-1) + 1);
		}
		
		function nextString(id) {
			var result = "";
			var carry = true;
			for (var i = id.length - 1; i >= 0; --i) {
				var digit = nextChar(id.charAt(i), i != 0);
				carry = digit.length == 0;
				if (carry)
					digit = 'a';
				result = digit + result;
				if (!carry) {
					result = id.substr(0, i) + result;
					break;
				}
			}
			if (carry)
				result = "a" + result;
			return result;
		}
		
		function nextSafeString(id) {
			do {
				id = nextString(id);
			} while (identUsage[id] > 0 || isBuiltin[id] || isProtected[id]);
			return id;
		}
		
		var replacement = "b";
		var macros = "";
		for (var i in identList) {
			var ident = identList[i];
			if (isProtected[ident])
				continue;
			if (identSavings[ident] < 1)
				continue;
			if (replacement.length > ident.length)
				continue;
			var backup = replacement;
			replacement = nextSafeString(replacement);
			if (replacement.length > ident.length) {
				// don't waste a perfectly fine id string
				replacement = backup;
				continue;
			}
			if (replacement === debugMissingSymbol) {
				WScript.Echo(ident + " => " + replacement);
			}
			if (needsMacro(ident)) {
				var macroMatch = source.match(RegExp('#define ' + ident + '\\b'));
				if (macroMatch) {
					source =
						source.substr(0, macroMatch.index) +
						"#define " + replacement +
						source.substr(macroMatch.lastIndex, source.length - macroMatch.lastIndex);
					
				} else {
					macros += "#define " + replacement + " " + ident + "\n";
				}
			}
			source = source.replace(RegExp("\\b" + ident + "\\b", "g"), replacement);
			//WScript.Echo(ident + ' => ' + replacement);
			//WScript.Echo(identUsage[ident] + " x " + ident + ": " +identSavings[ident]);
		}
		
		source = macros + source;
	}

	source = source.replace(/\n+$/g, "");
	
	var finalSize = source.length;
	var relativeSize = finalSize / initialSize;
	source = source.replace(/^(.*)$/gm, '"$1\\n"'); // escape newlines
	source = source.replace(/\\n\"$/g, '"'); // remove trailing newline
	
	var stats = srcPaths[srcIndex] + ": " + initialSize + " => " + finalSize + " (" + (100 * relativeSize).toFixed(1) + "%)";
	if (dstPath)
		WScript.Echo(stats);
	
	output += "\n";
	output += "// " + stats + "\n";
	output += "const char g_" + fso.GetBaseName(srcPaths[srcIndex]) + "[] =\n";
	output += source + ";\n";
}

////////////////////////////////////////////////////////////////

if (dstPath) {
	WScript.Echo("\nWriting " + dstPath);
	var outfile = fso.OpenTextFile(dstPath, ForWriting, ASCII);
	outfile.Write(output.replace(/\n/g, '\r\n'));
	outfile.Close();
} else {
	WScript.Echo(output);
}

var endTime = new Date();
if (dstPath) {
	WScript.Echo((endTime - startTime).valueOf() + " msec elapsed");
	for (var stage in timings) {
		WScript.Echo("   " + stage + ": " + timings[stage] + " msec");
	}
}
