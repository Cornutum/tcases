@echo off
rem #######################################################################
rem #
rem #                     Copyright 2012, Cornutum Project
rem #                              www.cornutum.org
rem # 
rem #                             $Revision$
rem #          $Date$
rem # 
rem #######################################################################

set TCASES_HOME=%~dp0..
set TCASES_LIB=%TCASES_HOME%\lib
set TCASES_ARGS=
set TCASES_LOG=tcases.log

:argsRead
if "%1"=="" goto argsDone
if "%1"=="-l" goto argsLogFile
set TCASES_ARGS=%TCASES_ARGS% %1
goto argsNext

:argsLogFile
shift
TCASES_LOG=%1
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
java -cp "%TCASES_CP%" -Dtcases.log.file=%TCASES_LOG% org.cornutum.tcases.Tcases %TCASES_ARGS%
