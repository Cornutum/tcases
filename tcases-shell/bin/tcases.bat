@echo off
rem #######################################################################
rem #
rem #                     Copyright 2012, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set TCASES_ARGS=
set TCASES_LOG_DEST=tcases.log.file
set TCASES_LOG=tcases.log
set TCASES_LOG_LEVEL=INFO

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
if "%1"=="-L" goto argsLogLevel
set TCASES_ARGS=%TCASES_ARGS% %1
goto argsNext

:argsLogFile
shift
TCASES_LOG_DEST=tcases.log.file
TCASES_LOG=%1
if not "%1"=="stdout" goto argsNext
TCASES_LOG_DEST=tcases.log.dest
TCASES_LOG=STDOUT
goto argsNext

:argsLogLevel
shift
TCASES_LOG_LEVEL=%1
goto argsNext

:argsNext
shift
goto argsRead

:argsDone

set TCASES_CP=%TCASES_LIB%
for %%j in (%TCASES_LIB%\*.jar) do call :cpConcat %%j
goto tcasesRun

:cpConcat
set TCASES_CP=%TCASES_CP%;%1
goto :eof

:tcasesRun
java -cp "%TCASES_CP%" -D%TCASES_LOG_DEST%=%TCASES_LOG% -Dtcases.log.level=%TCASES_LOG_LEVEL% org.cornutum.tcases.Tcases %TCASES_ARGS%
