@echo off
rem #######################################################################
rem #
rem #                     Copyright 2014, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set REDUCER_ARGS=
set REDUCER_LOG_DEST=tcases.log.file
set REDUCER_LOG=reducer.log
set REDUCER_LOG_LEVEL=INFO

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
if "%1"=="-L" goto argsLogLevel
set REDUCER_ARGS=%REDUCER_ARGS% %1
goto argsNext

:argsLogFile
shift
set REDUCER_LOG_DEST=tcases.log.file
set REDUCER_LOG=%1
if not "%1"=="stdout" goto argsNext
set REDUCER_LOG_DEST=tcases.log.dest
set REDUCER_LOG=STDOUT
goto argsNext

:argsLogLevel
shift
set REDUCER_LOG_LEVEL=%1
goto argsNext

:argsNext
shift
goto argsRead

:argsDone

set REDUCER_CP=%TCASES_LIB%
for %%j in (%TCASES_LIB%\*.jar) do call :cpConcat %%j
goto reducerRun

:cpConcat
set REDUCER_CP=%REDUCER_CP%;%1
goto :eof

:reducerRun
java -cp "%REDUCER_CP%" -D%REDUCER_LOG_DEST%=%REDUCER_LOG% -Dtcases.log.level=%REDUCER_LOG_LEVEL% org.cornutum.tcases.ReducerCommand %REDUCER_ARGS%
