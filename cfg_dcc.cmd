@echo off

rem Common options

  set UsePack=0
  set DEBUG=0
  set MAPFILE=0
  set JDBG=0
  set CleanDcc32Log=1
  set DEBUG_BATCH=0
  set TRACE_STACK_SOURCE=0

  set UserLib=.\..
  @rem set UserLib=.\..;.\..\comp
  @rem set UserLibI=%UserLib%
  @rem set UserLibR=%UserLib%

  @rem dcc analyze result options:
  @rem @set IGNOREERRORLEVEL=1

  @if not exist ".\_dcu\" md ".\_dcu"
  @if not exist ".\_dcu\" goto :eof
  @set UserCOpt=%UserCOpt% -N0.\_dcu

  @rem @if "%plfm%"=="w64" set UserCOpt=%UserCOpt% -E.\..\bin\x64
  @rem @if "%plfm%"=="w32" @set UserCOpt=%UserCOpt% -E.\..\bin

  @goto :eof
