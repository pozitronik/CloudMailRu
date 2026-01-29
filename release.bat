@echo off
setlocal enabledelayedexpansion

echo ============================================
echo MailRuCloud Release Build Script
echo ============================================

:: Change to script directory
cd /d "%~dp0"

:: ============================================
:: Step 1: Check for git tag on HEAD
:: ============================================
echo.
echo Checking for git tag on HEAD...

for /f "tokens=*" %%i in ('git tag --points-at HEAD 2^>nul') do set "GIT_TAG=%%i"

if "%GIT_TAG%"=="" (
    echo ERROR: No git tag found on HEAD.
    echo Please create a tag before building a release:
    echo   git tag vDDMMYYYY
    exit /b 1
)

echo Found tag: %GIT_TAG%

:: Extract version from tag (remove 'v' prefix if present)
set "VERSION=%GIT_TAG%"
if "%VERSION:~0,1%"=="v" set "VERSION=%VERSION:~1%"
if "%VERSION:~0,1%"=="V" set "VERSION=%VERSION:~1%"

echo Version: %VERSION%

:: ============================================
:: Step 2: Set up RAD Studio environment
:: ============================================
echo.
echo Setting up RAD Studio environment...

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
if errorlevel 1 (
    echo ERROR: Failed to set RAD Studio environment
    exit /b 1
)

:: ============================================
:: Step 3: Compile Win32 Release
:: ============================================
echo.
echo Compiling Win32 Release Build...

msbuild MailRuCloud.dproj /t:Build /p:Config=Release /p:Platform=Win32 /v:m /nologo
if errorlevel 1 (
    echo ERROR: Win32 Release build failed
    exit /b 1
)

echo Win32 Release build successful.

:: ============================================
:: Step 4: Compile Win64 Release
:: ============================================
echo.
echo Compiling Win64 Release Build...

msbuild MailRuCloud.dproj /t:Build /p:Config=Release /p:Platform=Win64 /v:m /nologo
if errorlevel 1 (
    echo ERROR: Win64 Release build failed
    exit /b 1
)

echo Win64 Release build successful.

:: ============================================
:: Step 5: Verify build outputs exist
:: ============================================
echo.
echo Verifying build outputs...

if not exist "Win32\Release\MailRuCloud.wfx" (
    echo ERROR: Win32\Release\MailRuCloud.wfx not found
    exit /b 1
)

if not exist "Win64\Release\MailRuCloud.wfx64" (
    echo ERROR: Win64\Release\MailRuCloud.wfx64 not found
    exit /b 1
)

echo Build outputs verified.

:: ============================================
:: Step 6: Create pluginst.inf
:: ============================================
echo.
echo Creating pluginst.inf...

(
echo [plugininstall]
echo description=CloudMailRu ^(32bit+64bit^)
echo type=wfx
echo file=MailRuCloud.wfx
echo defaultdir=CloudMailRu
echo version=%VERSION%
) > pluginst.inf

echo pluginst.inf created with version %VERSION%.

:: ============================================
:: Step 7: Create release archive
:: ============================================
echo.
echo Creating release archive...

:: Delete existing archive if present
if exist "CloudMailRu.zip" del /f "CloudMailRu.zip"

:: Create zip using PowerShell
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
    "$tempDir = 'release_temp'; " ^
    "if (Test-Path $tempDir) { Remove-Item -Recurse -Force $tempDir }; " ^
    "New-Item -ItemType Directory -Path $tempDir | Out-Null; " ^
    "Copy-Item -Path 'icons' -Destination $tempDir -Recurse; " ^
    "Copy-Item -Path 'x32' -Destination $tempDir -Recurse; " ^
    "Copy-Item -Path 'x64' -Destination $tempDir -Recurse; " ^
    "Copy-Item -Path 'Win32\Release\MailRuCloud.wfx' -Destination $tempDir; " ^
    "Copy-Item -Path 'Win64\Release\MailRuCloud.wfx64' -Destination $tempDir; " ^
    "Copy-Item -Path 'pluginst.inf' -Destination $tempDir; " ^
    "Copy-Item -Path 'README.MD' -Destination $tempDir; " ^
    "Compress-Archive -Path \"$tempDir\*\" -DestinationPath 'CloudMailRu.zip' -Force; " ^
    "Remove-Item -Recurse -Force $tempDir"

if errorlevel 1 (
    echo ERROR: Failed to create release archive
    exit /b 1
)

if not exist "CloudMailRu.zip" (
    echo ERROR: CloudMailRu.zip was not created
    exit /b 1
)

echo.
echo ============================================
echo Release build completed successfully!
echo Archive: CloudMailRu.zip
echo Version: %VERSION%
echo ============================================

endlocal
