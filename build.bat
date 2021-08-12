@echo off
setlocal EnableDelayedExpansion
pushd %~dp0

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set os_bits=64
if /i "%PROCESSOR_ARCHITECTURE%" == "x86" (
	if not defined PROCESSOR_ARCHITEW6432 set os_bits=32
)

set out_folder=output\crinkler\
set exe_path=!out_folder!demo.exe
set size_log=size_history.txt
set crinkler=external\crinkler23\win%os_bits%\crinkler.exe
set libs=user32.lib kernel32.lib gdi32.lib opengl32.lib winmm.lib
set range_libs=/RANGE:opengl32

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set mode=fast
set iterations=100000
if /i "%1" == "improve" set mode=improve
if /i "%1" == "stable" set mode=stable
if /i "%1" == "repro" set mode=repro

set crinkler_options=/COMPMODE:FAST
set crinkler_reuse=WRITE
set reuse_file=%out_folder%reuse.dat

if %mode% == improve (
	set crinkler_reuse=IMPROVE
	if not "%2" == "" (
		set iterations=%2
	)
	set crinkler_options=/COMPMODE:FAST /ORDERTRIES:!iterations! /HASHTRIES:1000
)

if %mode% == stable (
	set crinkler_reuse=STABLE
)

if %mode% == repro (
	set crinkler_reuse=STABLE
	set reuse_file=repro.dat
	if not "%2" == "" (
		set reuse_file=%2
	)
	if not exist !reuse_file! (
		echo ERROR: Repro file '!reuse_file!' not found.
		exit /b 1
	)
)

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Based on https://stackoverflow.com/a/9422268
set interactive=0
for /f "usebackq delims=" %%a in (`echo %cmdcmdline% ^| find /i /c /v "%~dpn0"`) do set interactive=%%a

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set wmic=%systemroot%\System32\wbem\wmic
for /f "delims=. " %%a in ('%wmic% os get LocalDateTime ^| findstr ^[0-9]') do (set ts=%%a)
set isodate=%ts:~0,4%-%ts:~4,2%-%ts:~6,2%T%ts:~8,2%:%ts:~10,2%

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

call %~dp0scripts\setup_env.bat
if ERRORLEVEL 1 exit /b 1

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:work

set obj_folder=!out_folder!obj\
if not exist "!obj_folder!" md "!obj_folder!"
set obj_files=
for %%f in (src\demo\*.cpp) do (
	:: https://docs.microsoft.com/en-us/cpp/build/reference/compiler-options-listed-alphabetically
 	cl /std:c++latest /Oi /arch:IA32 /QIfist /W1 /Zc:wchar_t /Zi /O2 /Oy /GS- /Zc:inline /fp:fast /D "NDEBUG" /errorReport:prompt /GF /WX- /Zc:forScope /Gs1048576 /GR- /Gr /Gw /MD /FC /nologo /diagnostics:classic /Fo!obj_folder! /c %%f
	::clang-cl /std:c++latest /Oi /arch:IA32 --target=i686-pc-windows-msvc /QIfist /W1 /Zc:wchar_t /Zi /O1 /Oy /GS- /Zc:inline /fp:fast /D "NDEBUG" /errorReport:prompt /GF /WX- /Zc:forScope /Gs1048576 /GR- /Gr /Gw /MD /FC /nologo /diagnostics:classic /Fo!obj_folder! /c %%f
	if ERRORLEVEL 1 goto clerror
	set obj_files=!obj_files! !obj_folder!%%~nf.obj
)

%crinkler% /PRIORITY:IDLE /SUBSYSTEM:WINDOWS /LARGEADDRESSAWARE /CRINKLER /TRUNCATEFLOATS %crinkler_options% /SATURATE /NOINITIALIZERS /TRANSFORM:CALLS %obj_files% %libs% %range_libs% /REPORT:!out_folder!report.html /REUSE:!reuse_file! /REUSEMODE:!crinkler_reuse! /NODEFAULTLIB /OUT:%exe_path%

set /p comment=Log comment: 
::for %%f in (%exe_path%) do @echo %isodate%,%%~zf,%comment%>>%size_log%
for %%f in (%exe_path%) do @echo %%~zf,%comment%>>%size_log%

goto end

:clerror
goto end

:end
if %interactive% EQU 0 (echo. && pause)
