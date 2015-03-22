@echo off

echo enter the server name:
set /p name=
erl +e 1000 +fnu +K true -pa ../ebin -name %name%@192.168.31.129 -s main -s reloader


