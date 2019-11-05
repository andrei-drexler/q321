Dim QUOTE, LF, TAB
QUOTE = Chr(34)
LF = "\n"
TAB = Chr(9)

Dim fso, infile, outfile, line, comment, whitespace, extraspace1, extraspace2

Const ForReading = 1, ForWriting = 2, ForAppending = 8
Const ASCII = 0
Set fso = CreateObject("Scripting.FileSystemObject")
Set infile = fso.OpenTextFile(WScript.Arguments(0))
Set outfile = fso.OpenTextFile(WScript.Arguments(1), ForAppending, ASCII)

Set comment = New RegExp
comment.Pattern = "//.*"

Set whitespace = New RegExp
whitespace.Pattern = "[ \t]+"
whitespace.Global = True

Set extraspace1 = New RegExp
extraspace1.Pattern = "([^a-zA-Z0-9]) "
extraspace1.Global = True

Set extraspace2 = New RegExp
extraspace2.Pattern = " ([^a-zA-Z0-9])"
extraspace2.Global = True

outfile.WriteLine()
outfile.WriteLine("const char g_" & fso.GetBaseName(WScript.Arguments(0)) & "[] =")

Dim was_comment
was_comment = False
While Not infile.AtEndOfStream
	line = infile.ReadLine
	line = Replace(line, TAB, " ")
	line = comment.Replace(line, "")
	line = whitespace.Replace(line, " ")
	line = extraspace1.Replace(line, "$1")
	line = extraspace2.Replace(line, "$1")
	line = Trim(line)
	If Len(line) > 0 Then
		line = Replace(line, "\", "\\")
		line = Replace(line, QUOTE, "\" & QUOTE)
		If InStr(1, line, "#") = 1 Then
			If Not was_comment Then
				line = LF & line
			End If
			line = line & LF
			was_comment = True
		Else
			'If Right(line, 1) 
			'line = line
		End If
'		line = QUOTE & line & LF & QUOTE
		line = QUOTE & line & QUOTE
		outfile.WriteLine(line)
	End If
Wend
outfile.WriteLine(";")

infile.Close
outfile.Close