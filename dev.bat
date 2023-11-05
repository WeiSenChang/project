@echo off

set cookie=weisenchang
set node=weisenchang@192.168.0.199

goto wait_input

:wait_input
    chcp 936
    echo =================
    echo ���ɼ�¼   record
    echo �����ļ�   make
    echo ��������   start
    echo ֹͣ����   stop
    echo ���´���   u
    echo ���ӷ���   attach
    echo �˳��ű�   quit
    echo =================
    set /p var=������ָ��:
    if "%var%" == "record" goto record
    if "%var%" == "make" goto make
    if "%var%" == "start" goto start
    if "%var%" == "stop" goto stop
    if "%var%" == "u" goto u
    if "%var%" == "quit" goto quit
    if "%var%" == "attach" goto attach
    goto wait_input

:record
    set var=
    escript make_table.erl
    goto wait_input

:make
    set var=
    rd /s /q "./ebin"
    md "./ebin"
    erl -noshell -s make all -s init stop
    copy ".\src\app\server.app" ".\ebin\"
    goto wait_input

:start
    set var=
    start werl -setcookie '%cookie%' -name %node% -pa ebin -s main
    goto wait_input

:stop
    set var=
    erl -setcookie '%cookie%' -name stop_%node% -pa ebin -eval "rpc:call('%node%', main, stop, []), rpc:call('%node%', init, stop, []), init:stop()"
    goto wait_input

:u
    set var=
    erl -setcookie '%cookie%' -name stop_%node% -pa ebin -eval "rpc:call('%node%', u, u, []), init:stop()"
    goto wait_input

:attach
    set var=
    set tick=%date:~0,4%%date:~5,2%%date:~8,2%%time:~0,2%%time:~3,2%%time:~6,2%
    echo %tick%
    start werl -setcookie '%cookie%' -name attach_%tick%_%node% -remsh %node%
    goto wait_input

:quit
    set var=
    goto :eof