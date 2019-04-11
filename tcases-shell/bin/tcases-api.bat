@echo off
rem #######################################################################
rem #
rem #                     Copyright 2019, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set API_ARGS=
set API_LOG_DEST=tcases.log.file
set API_LOG=tcases-api.log
set API_LOG_LEVEL=INFO

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
if "%1"=="-L" goto argsLogLevel
set API_ARGS=%API_ARGS% %1
goto argsNext

:argsLogFile
shift
set API_LOG_DEST=tcases.log.file
set API_LOG=%1
if not "%1"=="stdout" goto argsNext
set API_LOG_DEST=tcases.log.dest
set API_LOG=STDOUT
goto argsNext

:argsLogLevel
shift
set API_LOG_LEVEL=%1
goto argsNext

:argsNext
shift
goto argsRead

:argsDone

set API_CP=%TCASES_LIB%
for %%j in (%TCASES_LIB%\*.jar) do call :cpConcat %%j
goto apiRun

:cpConcat
set API_CP=%API_CP%;%1
goto :eof

:apiRun
java -cp "%API_CP%" -D%API_LOG_DEST%=%API_LOG% -Dtcases.log.level=%API_LOG_LEVEL% org.cornutum.tcases.openapi.ApiCommand %API_ARGS%
