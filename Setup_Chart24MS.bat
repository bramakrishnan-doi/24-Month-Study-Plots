@echo off
setlocal
cd /d "%~dp0"

set "RSCRIPT="
where Rscript.exe >nul 2>nul
if %errorlevel%==0 set "RSCRIPT=Rscript.exe"

if defined RSCRIPT goto found_r

for /d %%D in ("%ProgramFiles%\R\R-*" "%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
for /d %%D in ("%ProgramFiles%\R\R-*" "%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"

:found_r
if not defined RSCRIPT (
  echo.
  echo ERROR: Rscript.exe was not found.
  echo Install R for Windows from https://cran.r-project.org/ and try again.
  echo.
  pause
  exit /b 1
)

echo Installing/checking Chart24MS R packages...
"%RSCRIPT%" --vanilla setup.R
if errorlevel 1 (
  echo.
  echo Setup encountered an error. Review the messages above.
  echo.
  pause
  exit /b 1
)

echo.
echo Setup completed successfully.
echo You can now double-click Start_Chart24MS.bat.
echo.
pause
endlocal
