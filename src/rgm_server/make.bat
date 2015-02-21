@echo off

mkdir ebin

erl -pa ebin -make

cp src/interface/*.app ebin

pause
