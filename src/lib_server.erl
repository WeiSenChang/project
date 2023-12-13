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
    Starteds = get_starteds(Count),
    case Starteds >= length(?SERVERS) of
        true ->
            lib_cache:set_server_state(?SERVER, ?SERVER_STARTED);
        false ->
            timer:sleep(1000),
            wait_start_end(Count + 1)
    end.


get_starteds(Count) ->
    get_starteds(Count, 0, ?SERVERS).
get_starteds(_Count, Starteds, []) ->
    Starteds;
get_starteds(Count, Starteds, [Server | Tail]) ->
    State = lib_cache:get_server_state(Server),
    NewStarteds =
        case State of
            ?SERVER_STARTED ->
                Starteds + 1;
            ?SERVER_STARTING ->
                ?INFO("~w starting, ~w s", [Server, Count]),
                Starteds;
            _ ->
                Starteds
        end,
    get_starteds(Count, NewStarteds, Tail).

stop() ->
    lib_cache:set_server_state(?SERVER, ?SERVER_NO_START),
    OnLineRoleMap = lib_cache:get_online_role_map(),
    online_role_logout(maps:to_list(OnLineRoleMap)),
    ok.

online_role_logout([]) ->
    ok;
online_role_logout([{RoleId, _} | Tail]) ->
    lib_login:logout(RoleId),
    online_role_logout(Tail).