@echo off
rem #######################################################################
rem #
rem #                     Copyright 2022, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set COPY_ARGS=
set COPY_LOG_DEST=tcases.log.file
set COPY_LOG=copy.log
set COPY_LOG_LEVEL=INFO

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
if "%1"=="-L" goto argsLogLevel
set COPY_ARGS=%COPY_ARGS% %1
goto argsNext

:argsLogFile
shift
set COPY_LOG_DEST=tcases.log.file
set COPY_LOG=%1
if not "%1"=="stdout" goto argsNext
set COPY_LOG_DEST=tcases.log.dest
set COPY_LOG=STDOUT
goto argsNext

:argsLogLevel
shift
set COPY_LOG_LEVEL=%1
goto argsNext

:argsNext
shift
goto argsRead

:argsDone

set COPY_CP=%TCASES_LIB%
for %%j in ("%TCASES_LIB%"\*.jar) do call :cpConcat "%%j"
goto copyRun

:cpConcat
set COPY_CP=%COPY_CP%;%~1
goto :eof

:copyRun
java -cp "%COPY_CP%" -D%COPY_LOG_DEST%=%COPY_LOG% -Dtcases.log.level=%COPY_LOG_LEVEL% org.cornutum.tcases.CopyCommand %COPY_ARGS%
