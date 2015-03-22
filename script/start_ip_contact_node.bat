@echo off

echo enter the number:
set /p num=
erl -name contact%num%@192.168.31.129


