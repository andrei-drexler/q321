if WScript.Arguments.Count = 0 then
    WScript.Echo "Missing parameters"
    WScript.Quit 1
end if

dim sURL: sURL = WScript.Arguments(0)
dim sFileName: sFileName = Mid(sURL, InStrRev(sURL, "/") + 1)
dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

if WScript.Arguments.Count >= 2 then
    sFileName = WScript.Arguments(1)
end if

WScript.Echo "Downloading " & sURL & "..."

xHttp.Open "GET", sURL, False
xHttp.Send

with bStrm
    .type = 1 '//binary
    .open
    .write xHttp.responseBody
    .savetofile sFileName, 2 '//overwrite
end with