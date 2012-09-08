@echo off

IF "%1"== "test" GOTO :test
IF "%1"=="run" GOTO :run
IF "%1"=="clean" GOTO :clean
ECHO Cannot process command '%1'
GOTO :eof

:test
cabal configure
cabal build

:run
"dist/build/guesser/guesser.exe"
GOTO :eof

:clean
RD /S /Q dist