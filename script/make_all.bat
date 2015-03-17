@echo off

cd ..

rm -rf ebin

mkdir ebin

cp src/apps/*/src/*.app ebin/
cp src/*.app ebin/

erl -pa ebin  -make

pause
