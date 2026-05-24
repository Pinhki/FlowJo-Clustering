@echo off
set "BOOT_DIR=%~dp0"
if /I not "%~1"=="--keep-open" if /I not "%BOOT_DIR:~0,2%"=="\\" (
    cmd /v:on /k ""%~f0" --keep-open"
    exit /b
)
if /I "%~1"=="--keep-open" shift

setlocal EnableExtensions EnableDelayedExpansion
TITLE scRNA-seq Pipeline Launcher

set "SCRIPT_DIR=%~dp0"
set "TEMP_MAPPED_DRIVE="
set "DIRECT_RUN_SCRIPT="
set "LAUNCHER_VERSION=2026-04-03-r-4.4.3-enforced"
set "UNC_MODE=0"
set "RSCRIPT_PATH="
set "LAUNCH_ROOT="
set "CONTENT_DIR=Content"
set "LOCAL_CANDIDATE="
set "EXPECTED_R_VERSION=4.4.3"
set "FOUND_R_MISMATCH_PATH="
set "FOUND_R_MISMATCH_VERSION="
set "LOCAL_R_ROOT="
set "LOCAL_RSCRIPT_A="
set "LOCAL_RSCRIPT_B="

REM --- Prefer local path when launched from \\Mac\Home\... ---
if /I "%SCRIPT_DIR:~0,11%"=="\\Mac\Home\" (
    call :resolve_local_from_mac_home "%SCRIPT_DIR%"
    if defined LOCAL_CANDIDATE (
        if exist "!LOCAL_CANDIDATE!Contents\run.R" (
            set "SCRIPT_DIR=!LOCAL_CANDIDATE!"
            set "CONTENT_DIR=Contents"
            set "UNC_MODE=0"
            echo Detected local mirror of shared path:
            echo !SCRIPT_DIR!
        ) else if exist "!LOCAL_CANDIDATE!Content\run.R" (
            set "SCRIPT_DIR=!LOCAL_CANDIDATE!"
            set "CONTENT_DIR=Content"
            set "UNC_MODE=0"
            echo Detected local mirror of shared path:
            echo !SCRIPT_DIR!
        )
    )
)

REM --- Working directory setup (NO STAGING) ---
if /I "%SCRIPT_DIR:~0,2%"=="\\" (
    set "UNC_MODE=1"
    call :try_map_unc "%SCRIPT_DIR%"
    if errorlevel 1 (
        pushd "%SCRIPT_DIR%" >nul 2>nul
        if errorlevel 1 (
            echo.
            echo ======================== WARNING ======================
            echo Could not switch to launcher directory via map/pushd.
            echo Staging is disabled by policy.
            echo Source: %SCRIPT_DIR%
            echo Falling back to direct UNC script execution.
            echo =======================================================
            if exist "%SCRIPT_DIR%Contents\run.R" (
                set "DIRECT_RUN_SCRIPT=%SCRIPT_DIR%Contents\run.R"
                set "CONTENT_DIR=Contents"
            ) else (
                set "DIRECT_RUN_SCRIPT=%SCRIPT_DIR%Content\run.R"
                set "CONTENT_DIR=Content"
            )
        )
    )
) else (
    pushd "%SCRIPT_DIR%" >nul 2>nul
    if errorlevel 1 (
        echo.
        echo ======================== WARNING ======================
        echo Could not switch to launcher directory:
        echo %SCRIPT_DIR%
        echo =======================================================
        if exist "%SCRIPT_DIR%Contents\run.R" (
            set "DIRECT_RUN_SCRIPT=%SCRIPT_DIR%Contents\run.R"
            set "CONTENT_DIR=Contents"
        ) else (
            set "DIRECT_RUN_SCRIPT=%SCRIPT_DIR%Content\run.R"
            set "CONTENT_DIR=Content"
        )
    )
)

if exist "%SCRIPT_DIR%Contents\run.R" (
    set "CONTENT_DIR=Contents"
) else if exist "%SCRIPT_DIR%Content\run.R" (
    set "CONTENT_DIR=Content"
)

set "LAUNCH_ROOT=%CD%"
echo Launcher root: %LAUNCH_ROOT%
set "LOCAL_R_ROOT=%LAUNCH_ROOT%\runtime\R-%EXPECTED_R_VERSION%"
set "LOCAL_RSCRIPT_A=%LOCAL_R_ROOT%\bin\Rscript.exe"
set "LOCAL_RSCRIPT_B=%LOCAL_R_ROOT%\bin\x64\Rscript.exe"

echo ==========================================================
echo  Starting the Single-Cell RNA-Seq Analysis Pipeline...
echo ==========================================================
echo Launcher version: %LAUNCHER_VERSION%
echo.

call :locate_expected_rscript
if not errorlevel 1 goto :launch

call :maybe_install_local_r
if errorlevel 1 goto :error_handler

call :locate_expected_rscript
if not errorlevel 1 goto :launch
goto :error_handler

:launch
echo.
echo Using Rscript at: !RSCRIPT_PATH!

if defined DIRECT_RUN_SCRIPT goto :launch_unc_direct

if not exist "%CONTENT_DIR%\run.R" (
    echo.
    echo ======================== ERROR ========================
    echo Missing required file: %CONTENT_DIR%\run.R
    echo =======================================================
    goto :end_script
)
if not exist "%CONTENT_DIR%\app.R" (
    echo.
    echo ======================== ERROR ========================
    echo Missing required file: %CONTENT_DIR%\app.R
    echo =======================================================
    goto :end_script
)

if "%UNC_MODE%"=="1" (
    echo Skipping syntax preflight in UNC mode - network read instability workaround.
) else (
    echo Running syntax preflight on %CONTENT_DIR%\run.R and %CONTENT_DIR%\app.R...
    call :run_preflight
    if errorlevel 1 (
        echo.
        echo ======================== ERROR ========================
        echo Startup preflight failed.
        echo Please run from a local folder copy or fix mapped-drive file consistency.
        echo =======================================================
        goto :end_script
    )
)

echo Launching R script: %CONTENT_DIR%\run.R
echo ==========================================================
call "!RSCRIPT_PATH!" "%CONTENT_DIR%\run.R"
goto :end_script

:launch_unc_direct
echo Launching R script via UNC path fallback:
echo %DIRECT_RUN_SCRIPT%
echo ==========================================================
call "!RSCRIPT_PATH!" "!DIRECT_RUN_SCRIPT!"
goto :end_script

:run_preflight
"!RSCRIPT_PATH!" -e "parse(file='%CONTENT_DIR%/run.R'); parse(file='%CONTENT_DIR%/app.R'); cat('Preflight parse OK\n')"
exit /b %errorlevel%

:locate_expected_rscript
echo Searching for Rscript.exe (required version: %EXPECTED_R_VERSION%)...

REM 1) Project-local runtime first.
call :try_candidate "%LOCAL_RSCRIPT_A%" "project-local"
if not errorlevel 1 exit /b 0
call :try_candidate "%LOCAL_RSCRIPT_B%" "project-local"
if not errorlevel 1 exit /b 0

REM 2) PATH.
for /f "delims=" %%P in ('where Rscript.exe 2^>nul') do (
    call :try_candidate "%%P" "PATH"
    if not errorlevel 1 exit /b 0
)

REM 3) Program Files roots.
echo Searching C:\Program Files\R\...
call :find_rscript_in_root "C:\Program Files\R"
if not errorlevel 1 exit /b 0

set "PF86=%ProgramFiles(x86)%"
if defined PF86 (
    echo Searching !PF86!\R\...
    call :find_rscript_in_root "!PF86!\R"
    if not errorlevel 1 exit /b 0
)

exit /b 1

:try_candidate
set "CAND=%~1"
set "SRC=%~2"
if not exist "!CAND!" exit /b 1
set "CAND_VER="
for /f "tokens=1-4 delims= " %%A in ('"!CAND!" --version 2^>^&1') do (
    if /I "%%A %%B %%C"=="Rscript (R) version" if not defined CAND_VER set "CAND_VER=%%D"
)
if not defined CAND_VER set "CAND_VER=unknown"

if /I "!CAND_VER!"=="%EXPECTED_R_VERSION%" (
    set "RSCRIPT_PATH=!CAND!"
    echo Found compatible Rscript [!SRC!]: !CAND!  (R !CAND_VER!)
    exit /b 0
)

if not defined FOUND_R_MISMATCH_PATH (
    set "FOUND_R_MISMATCH_PATH=!CAND!"
    set "FOUND_R_MISMATCH_VERSION=!CAND_VER!"
)
echo Ignoring Rscript with mismatched version [!SRC!]: !CAND!  (R !CAND_VER!)
exit /b 1

:maybe_install_local_r
echo.
echo Required R version not found: %EXPECTED_R_VERSION%
if defined FOUND_R_MISMATCH_PATH (
    echo Detected different R version at:
    echo   !FOUND_R_MISMATCH_PATH!
    echo   version: !FOUND_R_MISMATCH_VERSION!
)
echo.
echo This launcher can install R %EXPECTED_R_VERSION% into this project folder:
echo   %LOCAL_R_ROOT%
choice /C YN /N /M "Install project-local R %EXPECTED_R_VERSION% now? [Y/N]: "
if errorlevel 2 (
    echo R installation was skipped by user.
    exit /b 1
)

set "R_INSTALLER=%TEMP%\R-%EXPECTED_R_VERSION%-win.exe"
set "R_URL=https://cran.r-project.org/bin/windows/base/old/%EXPECTED_R_VERSION%/R-%EXPECTED_R_VERSION%-win.exe"
echo Downloading R installer...
powershell -NoProfile -Command "$ProgressPreference='SilentlyContinue'; Invoke-WebRequest -Uri '%R_URL%' -OutFile '%R_INSTALLER%'"
if errorlevel 1 (
    echo ERROR: Failed to download R installer from:
    echo   %R_URL%
    exit /b 1
)

echo Installing R %EXPECTED_R_VERSION% to project-local runtime...
mkdir "%LOCAL_R_ROOT%" >nul 2>nul
start /wait "" "%R_INSTALLER%" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /DIR="%LOCAL_R_ROOT%"
if errorlevel 1 (
    echo ERROR: R installer failed.
    exit /b 1
)

call :try_candidate "%LOCAL_RSCRIPT_A%" "project-local"
if not errorlevel 1 exit /b 0
call :try_candidate "%LOCAL_RSCRIPT_B%" "project-local"
if not errorlevel 1 exit /b 0

echo ERROR: Installed Rscript not found or version mismatch after install.
exit /b 1

:detect_network_root
set "CHECK_PATH=%~1"
set "IS_NETWORK_ROOT=0"
if /I "%CHECK_PATH:~0,2%"=="\\" (
    set "IS_NETWORK_ROOT=1"
    exit /b 0
)
set "CHECK_DRIVE=%CHECK_PATH:~0,2%"
if not "%CHECK_DRIVE:~1,1%"==":" exit /b 0
for /f "usebackq delims=" %%L in (`net use 2^>nul`) do (
    echo(%%L | findstr /I /C:"%CHECK_DRIVE%" >nul
    if not errorlevel 1 (
        echo(%%L | findstr /I "\\\\" >nul
        if not errorlevel 1 (
            set "IS_NETWORK_ROOT=1"
            exit /b 0
        )
    )
)
exit /b 0

:resolve_local_from_mac_home
set "MAC_IN=%~1"
set "LOCAL_CANDIDATE="
if /I not "%MAC_IN:~0,11%"=="\\Mac\Home\" exit /b 0

set "MAC_REL=%MAC_IN:~11%"
if "%MAC_REL:~0,1%"=="\" set "MAC_REL=%MAC_REL:~1%"

if defined USERPROFILE (
    set "TRY_A=%USERPROFILE%\%MAC_REL%"
    if exist "!TRY_A!\" (
        set "LOCAL_CANDIDATE=!TRY_A!"
        exit /b 0
    )
)

if defined HOMEDRIVE if defined HOMEPATH (
    set "TRY_B=%HOMEDRIVE%%HOMEPATH%\%MAC_REL%"
    if exist "!TRY_B!\" (
        set "LOCAL_CANDIDATE=!TRY_B!"
        exit /b 0
    )
)

for %%D in (C D E F G H I J K L M N O P Q R S T U V W X Y Z) do (
    set "TRY_C=%%D:\%MAC_REL%"
    if exist "!TRY_C!\" (
        set "LOCAL_CANDIDATE=!TRY_C!"
        exit /b 0
    )
)
exit /b 0

:find_rscript_in_root
set "RROOT=%~1"
if not exist "!RROOT!" exit /b 1
for /f "delims=" %%I in ('dir /b /ad /o-n "!RROOT!\R-*" 2^>nul') do (
    call :try_candidate "!RROOT!\%%I\bin\x64\Rscript.exe" "installed-root"
    if not errorlevel 1 exit /b 0
    call :try_candidate "!RROOT!\%%I\bin\Rscript.exe" "installed-root"
    if not errorlevel 1 exit /b 0
)
exit /b 1

:try_map_unc
set "UNC_PATH=%~1"
if /I not "%UNC_PATH:~0,2%"=="\\" exit /b 1
echo UNC path detected. Attempting temporary drive mapping...
for %%D in (Z Y X W V U T S R Q P O N M L K J I H G F E D) do (
    if not exist "%%D:\" (
        net use %%D: "%UNC_PATH%" /persistent:no >nul 2>nul
        if not errorlevel 1 (
            set "TEMP_MAPPED_DRIVE=%%D:"
            pushd "%%D:\" >nul 2>nul
            if not errorlevel 1 (
                echo Mapped %UNC_PATH% to %%D:
                exit /b 0
            )
            popd >nul 2>nul
            net use %%D: /delete >nul 2>nul
        )
    )
)
exit /b 1

:error_handler
echo.
echo ======================== ERROR ========================
echo Required Rscript version %EXPECTED_R_VERSION% could not be resolved.
echo.
echo You can install R %EXPECTED_R_VERSION% manually from:
echo https://cran.r-project.org/bin/windows/base/
echo =======================================================

:end_script
popd >nul 2>nul
if defined TEMP_MAPPED_DRIVE net use %TEMP_MAPPED_DRIVE% /delete >nul 2>nul
echo.
echo The script has finished. Press any key to close this window.
pause
