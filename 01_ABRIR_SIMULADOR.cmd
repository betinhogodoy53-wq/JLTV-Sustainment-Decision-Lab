@echo off
setlocal EnableExtensions
set "ROOT=%~dp0"
set "APPDIR=%ROOT%02_CODIGO"
set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
for /f "tokens=2,*" %%A in ('reg query "HKCU\Software\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do if not defined RSCRIPT if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
for /f "tokens=2,*" %%A in ('reg query "HKLM\Software\R-core\R" /v InstallPath 2^>nul ^| find "InstallPath"') do if not defined RSCRIPT if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
for /f "delims=" %%D in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do if not defined RSCRIPT if exist "%ProgramFiles%\R\%%D\bin\Rscript.exe" set "RSCRIPT=%ProgramFiles%\R\%%D\bin\Rscript.exe"
if not defined RSCRIPT (
  echo R nao localizado. Instale o R para Windows e tente novamente.
  pause
  exit /b 1
)
cd /d "%APPDIR%"
"%RSCRIPT%" --vanilla install_dependencies.R --check
if not errorlevel 1 goto ABRIR
echo Faltam pacotes R. A instalacao exige conexao com a internet.
choice /c SN /m "Instalar os pacotes necessarios agora?"
if errorlevel 2 exit /b 10
"%RSCRIPT%" --vanilla install_dependencies.R
if errorlevel 1 goto ERRO
:ABRIR
if /i "%~1"=="--verificar" (
  echo R e dependencias localizados.
  exit /b 0
)
if not exist "%ROOT%DADOS" mkdir "%ROOT%DADOS"
set "JLTV_DB_PATH=%ROOT%DADOS\jltv.sqlite"
set "JLTV_SEED_SYNTHETIC=0"
echo Abrindo JLTV Sustainment Decision Lab. Mantenha esta janela aberta.
"%RSCRIPT%" --vanilla run_app.R
if errorlevel 1 goto ERRO
exit /b 0
:ERRO
echo Nao foi possivel concluir. Consulte a mensagem acima e o log em 02_CODIGO.
pause
exit /b 1
