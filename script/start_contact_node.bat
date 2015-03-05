@echo off

echo enter the number:
set /p num=
erl -sname contact%num%@localhost


