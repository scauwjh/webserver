@echo off

cd ..
erl -pa ebin -make

cp src/rgm_server/src/interface/*.app ebin

pause
