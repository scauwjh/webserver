@echo off

erl +e 1000 +fnu +K true -pa ../ebin ../apps/rgm_server/ebin -sname webserver@localhost -s main

