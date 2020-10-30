@echo off
rem #######################################################################
rem #
rem #                     Copyright 2020, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set ANON_ARGS=
set ANON_LOG_DEST=tcases.log.file
set ANON_LOG=tcases-anon.log
set ANON_LOG_LEVEL=INFO

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
if "%1"=="-L" goto argsLogLevel
set ANON_ARGS=%ANON_ARGS% %1
goto argsNext

:argsLogFile
shift
set ANON_LOG_DEST=tcases.log.file
set ANON_LOG=%1
if not "%1"=="stdout" goto argsNext
set ANON_LOG_DEST=tcases.log.dest
set ANON_LOG=STDOUT
goto argsNext

:argsLogLevel
shift
set ANON_LOG_LEVEL=%1
goto argsNext

:argsNext
shift
goto argsRead

:argsDone

set ANON_CP=%TCASES_LIB%
for %%j in ("%TCASES_LIB%"\*.jar) do call :cpConcat "%%j"
goto anonRun

:cpConcat
set ANON_CP=%ANON_CP%;%~1
goto :eof

:anonRun
java -cp "%ANON_CP%" -D%ANON_LOG_DEST%=%ANON_LOG% -Dtcases.log.level=%ANON_LOG_LEVEL% org.cornutum.tcases.anon.AnonCommand %ANON_ARGS%
