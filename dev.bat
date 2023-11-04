@echo off

set cookie=weisenchang
set node=weisenchang@192.168.0.199

goto wait_input

:wait_input
    chcp 936
    echo =================
    echo 编译文件   make
    echo 启动服务   start
    echo 停止服务   stop
    echo 更新代码   u
    echo 连接服务   attach
    echo 退出脚本   quit
    echo =================
    set /p var=请输入指令:
    if "%var%" == "make" goto make
    if "%var%" == "start" goto start
    if "%var%" == "stop" goto stop
    if "%var%" == "u" goto u
    if "%var%" == "quit" goto quit
    if "%var%" == "attach" goto attach
    goto wait_input

:make
    set var=
    escript make_table.erl
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
    start werl -setcookie '%cookie%' -name attach_%node% -remsh %node%
    goto wait_input

:quit
    set var=
    goto :eof