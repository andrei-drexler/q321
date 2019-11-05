var TrimLF = true;

var ForReading = 1, ForWriting = 2, ForAppending = 8;
var ASCII = 0;

var fso = new ActiveXObject("Scripting.FileSystemObject");
var infile = fso.OpenTextFile(WScript.Arguments(0));
var outfile = fso.OpenTextFile(WScript.Arguments(1), ForAppending, ASCII);

outfile.WriteLine();
outfile.WriteLine("const char g_" + fso.GetBaseName(WScript.Arguments(0)) + "[] =");

var was_define = false;
while (!infile.AtEndOfStream) {
	var line = infile.ReadLine();
	line = line.replace(/\t+/g, " ");
	line = line.replace(/\/\/.*/, "");
	line = line.replace(/[ \t]+/, " ");
	line = line.replace(/([^a-zA-Z0-9]) /g, "$1");
	line = line.replace(/ ([^a-zA-Z0-9])/g, "$1");
	line = line.replace(/^\s+|\s+$/g, '');
	if (line.length == 0)
		continue;
	line = line.replace('\\', "\\\\");
	line = line.replace('"', '\\"');
	if (TrimLF) {
		var define = "#";
		if (line.length >= define.length && line.substr(0, define.length) == define) {
			if (!was_define)
				line = "\\n" + line;
			line = line + "\\n";
			was_define = true;
		} else {
			was_define = false;
			if (line.match(/[0-9a-z]$/i))
				line = line + "\\n";
		}
	} else {
		line = line + "\\n";
	}
	line = '"' + line + '"';
	outfile.WriteLine(line);
}
outfile.WriteLine(";");

infile.Close();
outfile.Close();
