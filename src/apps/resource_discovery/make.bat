@echo off

mkdir ebin

erl -pa ebin -make

cp src/*.app ebin

pause
