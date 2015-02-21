@echo off

erl +e 1000 +fnu +K true -pa ../ebin -sname webserver@localhost -s main -s reloader


