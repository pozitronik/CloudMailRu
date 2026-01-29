@echo off
setlocal enabledelayedexpansion

echo ============================================
echo MailRuCloud Build Script
echo ============================================

:: Change to script directory
cd /d "%~dp0"

:: ============================================
:: Step 1: Compile Win64 Debug
:: ============================================
echo.
echo Compiling Win64 Debug Build...

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
if errorlevel 1 (
    echo ERROR: Failed to set RAD Studio environment
    exit /b 1
)

msbuild MailRuCloud.dproj /t:Build /p:Config=Debug /p:Platform=Win64 /v:m /nologo
if errorlevel 1 (
    echo ERROR: Build failed
    exit /b 1
)

echo Build successful.

endlocal
