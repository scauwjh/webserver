@echo off

echo enter the server name(like: webserver@localhost):
set /p name=
erl +e 1000 +fnu +K true -pa ../ebin -sname %name% -s main -s reloader


