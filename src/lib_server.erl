%% -*- coding: utf-8 -*-

-module(lib_server).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    lib_ets:ets_init(),
    lib_db:db_init(),
    start_server().

start_server() ->
    lib_cache:set_server_state(?SERVER, ?SERVER_STARTING),
    start_server(?SERVERS),
    wait_start_end(0),
    ok.
start_server([]) ->
    ok;
start_server([Server | Tail]) ->
    server_sup:start_child(Server, Server, transient, []),
    start_server(Tail).

wait_start_end(Count) ->
    Starteds = get_starteds(),
    case Starteds >= length(?SERVERS) of
        true ->
            lib_cache:set_server_state(?SERVER, ?SERVER_STARTED);
        false ->
            timer:sleep(1000),
            wait_start_end(Count + 1)
    end.


get_starteds() ->
    get_starteds(0, ?SERVERS).
get_starteds(Starteds, []) ->
    Starteds;
get_starteds(Starteds, [Server | Tail]) ->
    State = lib_cache:get_server_state(Server),
    NewStarteds = ?IF(State =:= ?SERVER_STARTED, Starteds + 1, Starteds),
    get_starteds(NewStarteds, Tail).

stop() ->
    mnesia:stop(),
    ok.