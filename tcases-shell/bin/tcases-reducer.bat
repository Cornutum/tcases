@echo off
rem #######################################################################
rem #
rem #                     Copyright 2014, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #                             $Revision$
rem #          $Date$
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set REDUCER_ARGS=
set REDUCER_LOG=reducer.log

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
set REDUCER_ARGS=%REDUCER_ARGS% %1
goto argsNext

:argsLogFile
shift
REDUCER_LOG=%1
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
java -cp "%REDUCER_CP%" -Dtcases.log.file=%REDUCER_LOG% org.cornutum.tcases.Reducer %REDUCER_ARGS%
