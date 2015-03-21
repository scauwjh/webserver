@echo off

echo enter the number:
set /p num=
erl -name contact%num%@172.16.28.132


