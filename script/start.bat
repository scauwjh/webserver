@echo off

erl +e 1000 +fnu +K true -pa ../ebin -sname webserver -s main -s reloader


