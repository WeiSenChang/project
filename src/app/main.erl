%% -*- coding: utf-8 -*-

-module(main).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    ok = application:start(server),
    ok = start_server(),
    ?INFO("start servers end"),
    ok.


stop() ->
    ok = stop_server(lists:reverse(?SERVERS)),
    ok = application:stop(server).


%% 内部函数
start_server() ->
    start_server(?SERVERS),
    wait_start_end(0).
start_server([]) ->
    ok;
start_server([Server | Tail]) ->
    server_sup:start_child(Server, Server, transient, []),
    start_server(Tail).

wait_start_end(Count) ->
    Starteds = get_starteds(),
    case Starteds >= length(?SERVERS) of
        true ->
            ok;
        false ->
            timer:sleep(1000),
            wait_start_end(Count + 1)
    end.

stop_server([]) ->
    ok;
stop_server([Server | Tail]) ->
    server_sup:terminate_child(Server:get_p_name()),
    server_sup:delete_child(Server:get_p_name()),
    stop_server(Tail).


get_starteds() ->
    get_starteds(0, ?SERVERS).
get_starteds(Starteds, []) ->
    Starteds;
get_starteds(Starteds, [Server | Tail]) ->
    State = lib_cache:get_server_state(Server),
    NewStarteds = ?IF(State =:= ?SERVER_STARTED, Starteds + 1, Starteds),
    get_starteds(NewStarteds, Tail).