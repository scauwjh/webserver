@echo off

cd ..

mkdir ebin

cp src/apps/*/src/*.app ebin/
cp src/*.app ebin/

erl -pa ebin  -make

pause
