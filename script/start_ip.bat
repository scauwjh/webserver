@echo off

echo enter the server name:
set /p name=
erl +e 1000 +fnu +K true -pa ../ebin -name %name%@172.16.28.132 -s main -s reloader


