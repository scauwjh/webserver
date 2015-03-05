@echo off

echo enter the server name:
set /p name=
erl +e 1000 +fnu +K true -pa ../ebin -sname %name%@localhost -s main -s reloader


