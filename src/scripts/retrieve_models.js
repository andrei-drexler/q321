var fso = new ActiveXObject("Scripting.FileSystemObject");
var shell = WScript.CreateObject("WScript.Shell");
var shell_app = WScript.CreateObject("Shell.Application");
var script_dir = fso.GetParentFolderName(WScript.ScriptFullName);
var current_dir = shell.CurrentDirectory;

////////////////////////////////////////////////////////////////

function print(str) {
	WScript.StdOut.Write(str);
}

function prompt(msg, options) {
	options = options.toLowerCase();

	msg += ' [';
	for (var i = 0; i < options.length; ++i) {
		if (i)
			msg += '/';
		msg += options.charAt(i);
	}
	msg += ']: ';

	while (true) {
		print(msg);
		var response = WScript.StdIn.ReadLine();
		if (response.length != 1)
			continue;
		var c = response.charAt(0).toLowerCase();
		if (options.indexOf(c) != -1)
			return c;
	}
}

function startsWith(prefix, str) {
	return str.length >= prefix.length && str.substr(0, prefix.length) == prefix;
}

function endsWith(suffix, str) {
	return str.length >= suffix.length && str.substr(str.length - suffix.length, suffix.length) == suffix;
}

function parseModelList(path) {
	var infile = fso.OpenTextFile(path);
	var source = infile.ReadAll();
	infile.Close();

	source = source.replace(/\r/g, ""); // normalize newline sequences
	
	var model_def = source.match(/#define DEMO_MODELS\(x\)\s*\\\n([^\n]*\\\n)+/gm);
	if (!model_def) {
		WScript.Quit(1);
	}
	source = model_def[0];

	var models = {
		items: [],
		mapobjects: []
	};
	var entry_exp = /^\s*x\(\s*([a-z0-9\/]+)\s*\,\s*(\w+)\s*\)/gim, match;
	while ((match = entry_exp.exec(source)) !== null) {
		var path = match[1] + "/" + match[2] + ".md3";
		if (startsWith("models/mapobjects", match[1]))
			models.mapobjects.push(path);
		else
			models.items.push(path);
	}
	
	return models;
}

function runScript(script) {
	var Finished = 1;
	var Failed = 2;
	var proc = shell.Exec("cscript /nologo " + script);
	while (!proc.status)
		WScript.Sleep(100);
	var output = "";
	var result = proc.status;
	if (result == Finished)
		output = proc.StdOut.ReadAll();
	else if (result == Failed)
		output = proc.StdErr.ReadAll();
	print(output);
	return proc.ExitCode;
}

function download(url, output_path) {
	return runScript(script_dir + "/download.js -silent " + url + " " + output_path)
}

function getLevel(s) {
	var level = 0;
	for (var i = 0; i < s.length; ++i)
		if (s.charAt(i) == '/')
			++level;
	return level;
}

function comparePath(lhs, rhs) {
	var level_delta = getLevel(lhs) - getLevel(rhs);
	if (level_delta != 0)
		return level_delta;
	return lhs.localeCompare(rhs);
}

function normalizePath(path) {
	return path.replace(/\//g, '\\');
}

function createFolders(path) {
	for (var i = 0; i < path.length; ++i) {
		var c = path.charAt(i);
		if (c != '/' && c != '\\')
			continue;
		var partial = path.substring(0, i + 1);
		if (!fso.FolderExists(partial))
			fso.CreateFolder(partial);
	}
}

function trim(s) {
	return s.replace(/(?:^\s+)|(?:\s+$)/g, '');
}

function readRegKey(path) {
	try {
		return shell.RegRead(path);
	} catch (e) {
		return null;
	}
}

function getDemoPath() {
	var uninstall =
		readRegKey("HKEY_LOCAL_MACHINE\\SOFTWARE\\Wow6432Node\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Quake 3 Arena Demo\\UninstallString") ||
		readRegKey("HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\Quake 3 Arena Demo\\UninstallString");
	if (!uninstall)
		return null;
	var match = uninstall.match(/(?:.*\\unvise32\.exe)\s+(.*)(?:[\\\/]uninstal\.log)\s*/i);
	if (match)
		return match[1];
	return null;
}

////////////////////////////////////////////////////////////////

var models = parseModelList(script_dir + "/../demo/resource_def.h");
var num_retrieved = 0;
var num_needed_models = models.items.length + models.mapobjects.length;

var pak_path = fso.BuildPath(current_dir, "pak0.pk3");
var zip_path = fso.BuildPath(current_dir, "pak0.zip");
var is_temp_pak = false;

if (!fso.FileExists(pak_path) && !fso.FileExists(zip_path)) {
	var demo_path = getDemoPath();
	if (demo_path) {
		print("Found Quake 3 Arena Demo in " + demo_path + "\n");
		var demo_pak = demo_path + "/demoq3/pak0.pk3";
		if (fso.FileExists(demo_pak)) {
			fso.CopyFile(demo_pak, zip_path);
			is_temp_pak = true;
		}
	}
}

if (!fso.FileExists(pak_path) && !fso.FileExists(zip_path)) {
	var demo_url = "https://www.fileplanet.com/archive/p-16222/Quake3-Arena-Demo/download";
	print(
		"Some of the models need to be unzipped from a Quake 3 pak0.pk3 file.\n" +
		"If you already have Quake 3 installed (either demo or full version)\n" +
		"please copy pak0.pk3 to this folder and run this script again.\n" +
		"\n" +
		"If you don't have Quake 3 installed you can download the demo from\n" +
		demo_url + "\n" +
		"\n"
	);
	if (prompt("Open download page?", "yn") == 'y') {
		shell.Run("https://www.fileplanet.com/archive/p-16222/Quake3-Arena-Demo/download");
	}
	print("\n");
} else {
	print("Extracting models from pak0.pk3:\n");
	
	if (!is_temp_pak && !fso.FileExists(zip_path))
		fso.MoveFile(pak_path, zip_path);
	
	var items = shell_app.NameSpace(zip_path).Items();

	models.items.sort(comparePath);
	for (var i = 0; i < models.items.length; ++i) {
		var path = models.items[i];
		print("  " + path);
		path = normalizePath(path);
		var item = items.Item(path);
		if (!item) {
			print(" - missing!\n");
			continue;
		}
		print("\n");

		var dst_path = fso.BuildPath(current_dir, path);
		dst_path = fso.GetParentFolderName(dst_path) + '\\';
		createFolders(dst_path);
		var dst = shell_app.NameSpace(dst_path);
		// https://docs.microsoft.com/en-us/windows/win32/shell/folder-copyhere
		var copy_flags =
			+ 4 // no progress dialog
			+ 16 // yes to all
			+ 1024 // no error UI
		;
		dst.CopyHere(item, copy_flags);
		++num_retrieved;
	}

	if (is_temp_pak)
		fso.DeleteFile(zip_path);
	else
		fso.MoveFile(zip_path, pak_path);
}

print("Downloading mapobjects (from svn.icculus.org/gtkradiant-gamepacks/Q3Pack):\n");
models.mapobjects.sort(comparePath);

for (var i = 0; i < models.mapobjects.length; ++i) {
	var path = models.mapobjects[i];
	print("  " + path);
	var output_path = normalizePath(current_dir + "\\" + path);
	createFolders(fso.GetParentFolderName(output_path));
	var status = download("http://svn.icculus.org/*checkout*/gtkradiant-gamepacks/Q3Pack/trunk/install/baseq3/" + path, output_path);
	if (status != 0) {
		print(" - failed! (" + status + ")\n");
	} else {
		WScript.StdOut.Write("\n");
		++num_retrieved;
	}
}

print("Retrieved " + num_retrieved + "/" + num_needed_models + " models.\n\n");
