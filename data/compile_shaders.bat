@echo off
setlocal EnableDelayedExpansion
pushd %~dp0..

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set src_folder=data/shaders
set out_folder=src/demo/cooked
set out_file=%out_folder%/cooked_shaders.h
set compiler=%~dp0../output/Win32/Release/shader_compiler.exe

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Based on https://stackoverflow.com/a/9422268
set interactive=0
for /f "usebackq delims=" %%a in (`echo %cmdcmdline% ^| find /i /c /v "%~dpn0"`) do set interactive=%%a

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: skip costly dev env setup + compilation if possible
cscript /nologo scripts/compare_file_times.js src/demo/resource_def.h "!compiler!"
if not errorlevel 1 goto has_compiler

echo Setting up compilation environment...
call scripts\setup_env.bat
if NOT ERRORLEVEL 1 (
	echo Building shader compiler...
	msbuild /nologo /v:quiet src\tools\shader_compiler\shader_compiler.vcxproj /p:Configuration=Release /p:Platform=Win32 /p:SolutionDir=%~dp0../
)

echo.

if exist "%compiler%" goto has_compiler
echo ERROR: Shader compiler not found.
goto end

:has_compiler

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if not exist "%out_folder%" md "%out_folder%"
%compiler% %src_folder% %out_file%

:end
if %interactive% EQU 0 (echo. && pause)
