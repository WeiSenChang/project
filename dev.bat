@echo off

set cookie=weisenchang
set node=weisenchang@192.168.0.199

goto wait_input

:wait_input
    chcp 936
    echo =================
    echo �����ļ�   make
    echo ��������   start
    echo ֹͣ����   stop
    echo ���´���   u
    echo �˳��ű�   quit
    echo =================
    set /p var=������ָ��:
    if "%var%" == "make" goto make
    if "%var%" == "start" goto start
    if "%var%" == "stop" goto stop
    if "%var%" == "u" goto u
    if "%var%" == "quit" goto quit
    goto wait_input

:make
    set var=
    erl -noshell -s make all -s init stop
    goto wait_input

:start
    set var=
    start werl -setcookie '%cookie%' -name %node% -pa ebin -s main
    goto wait_input

:stop
    set var=
    erl -setcookie '%cookie%' -name stop_%node% -pa ebin -eval "rpc:call('%node%', init, stop, []), init:stop()"
    goto wait_input

:u
    set var=
    erl -setcookie '%cookie%' -name stop_%node% -pa ebin -eval "rpc:call('%node%', u, u, []), init:stop()"
    goto wait_input

:quit
    set var=
    goto :eof