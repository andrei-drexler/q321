@echo off
setlocal EnableDelayedExpansion
pushd %~dp0

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set src_folder=data/shaders
set out_folder=src/demo/cooked
set out_file=%out_folder%/cooked_shaders.h
set compiler=%~dp0output/Win32/Release/shader_compiler/shader_compiler.exe

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Based on https://stackoverflow.com/a/9422268
set interactive=0
for /f "usebackq delims=" %%a in (`echo %cmdcmdline% ^| find /i /c /v "%~dpn0"`) do set interactive=%%a

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

call src\scripts\setup_env.bat
if NOT ERRORLEVEL 1 msbuild /nologo /v:quiet src\tools\shader_compiler\shader_compiler.vcxproj /p:Configuration=Release /p:Platform=Win32 /p:SolutionDir=%~dp0

if exist "%compiler%" goto has_compiler
echo ERROR: Shader compiler not found.
goto end

:has_compiler

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if not exist "%out_folder%" md "%out_folder%"
%compiler% %src_folder% %out_file%

:end
if %interactive% EQU 0 (echo. && pause)
