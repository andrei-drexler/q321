var silent = false;
var url = null;
var output_name = null;

function showSyntax() {
	WScript.Echo("Syntax: " + WScript.ScriptName + " [-silent] <url> [destination]");
}

if (WScript.Arguments.length == 0) {
	showSyntax();
	WScript.Quit(1);
}

for (var arg_index = 0; arg_index < WScript.Arguments.length; ++arg_index) {
	var arg = WScript.Arguments(arg_index);
	if (arg == "-silent") {
		silent = true;
		continue;
	}
	if (!url) {
		url = arg;
		continue;
	}
	if (!output_name) {
		output_name = arg;
		continue;
	}
	if (!silent) {
		WScript.Echo("Skipping extra argument: " + arg);
	}
}

if (!url) {
	if (!silent) {
		WScript.Echo("ERROR: No URL specified.");
		showSyntax();
	}
	WScript.Quit(1);
}

if (!output_name) {
	var i = url.lastIndexOf('/');
	if (i != -1) {
		output_name = url.substring(i + 1);
	}
}

if (!output_name) {
	if (!silent) {
		WScript.Echo("ERROR: No output file name specified.");
		showSyntax();
	}
	WScript.Quit(1);
}

if (!silent) {
	WScript.Echo("Downloading " + url + " to " + output_name + "...");
}

var fso = new ActiveXObject("Scripting.FileSystemObject");
var target_folder = fso.GetParentFolderName(output_name);
if (!fso.FolderExists(target_folder))
	fso.CreateFolder(target_folder);

var http = new ActiveXObject("Microsoft.XMLHTTP");
http.Open("GET", url, false);
http.Send();

if (http.status != 200) {
	if (!silent) {
		var msg = "code " + http.status;
		if (http.statusText)
			msg += " - " + http.statusText;
		WScript.Echo("ERROR: download failed (" + msg + ")");
	}
	WScript.Quit(http.status || 404);
}

var stream = new ActiveXObject("Adodb.Stream");
stream.Type = 1; // binary;
stream.Open();
stream.Write(http.responseBody);
stream.SaveToFile(output_name, 2); // overwrite
stream.Close();
