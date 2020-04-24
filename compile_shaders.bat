@echo off
setlocal EnableDelayedExpansion

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

set src_folder=src/demo/data/shaders/
set sources=%src_folder%*.glsl
set out_folder=src/demo/cooked/
set out_file=%out_folder%cooked_shaders.h

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:: Based on https://stackoverflow.com/a/9422268
set interactive=0
for /f "usebackq delims=" %%a in (`echo %cmdcmdline% ^| find /i /c /v "%~dpn0"`) do set interactive=%%a

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if not exist "%out_folder%" md "%out_folder%"

set source_list=
for %%f in (%sources%) do (
	set source_list=!source_list! !src_folder!%%~nxf
)
cscript /nologo src/scripts/cook_shader.js !source_list! -o !out_file!

:end
if %interactive% EQU 0 (echo. && pause)
