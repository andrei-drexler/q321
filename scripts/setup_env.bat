@echo off

set check_cl=where cl.exe ^>nul 2^>nul
%check_cl% && exit /b 0

:: Not running from a VS Developer Command Prompt, need to set up environment manually

:: VS2017 or newer
set vs2017_where="%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if exist %vs2017_where% goto try_vs2017
goto no_env

:try_vs2017
for /f "usebackq delims=#" %%a in (`%vs2017_where% -latest -property installationPath`) do call "%%a\Common7\Tools\VsDevCmd.bat" >nul

%check_cl% && exit /b 0

:no_env
echo ERROR: Could not setup Visual C++ environment.
echo Please run this script from a VS Developer Command Prompt.
exit /b 1
