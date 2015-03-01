@echo off

cd ..

mkdir ebin

cp src/apps/*/src/*.app ebin/

erl -pa ebin  -make

pause
