@echo off

rem build
erl -pa ebin -make

rem app files
cp src/interface/*.app ebin

pause
