%% coding: utf-8
-module(lib_server).
-author("weisenchang").

-include("common.hrl").

%% API
-export([start/0, stop/0, set_server_state/2]).

-define(ETS_SERVER_STATE, ets_server_state).
-define(START_TICK, start_tick).

-define(SERVERS, [mod_mnesia, mod_timer, mod_log, mod_counter, mod_role_manage, mod_friend]).

start() ->
    set_start_tick(),
    ets:new(?ETS_SERVER_STATE, [named_table, public]),
    db_mnesia:init_db(),
    lists:foreach(
        fun(Server) ->
            set_server_state(Server, ?SERVER_NO_START),
            server_sup:start_child(Server, Server, transient, [])
        end, ?SERVERS),
    wait_start_end(1),
    ok.

wait_start_end(Count) ->
    ServersStarted = lists:all(
        fun(Server) ->
            case ets:lookup(?ETS_SERVER_STATE, Server) of
                [{Server, State}] ->
                    State =:= ?SERVER_STARTED;
                _ ->
                    false
            end
        end, ?SERVERS),
    case ServersStarted of
        true ->
            StartTick = get_start_tick(),
            EndTick = lib_timer:unix_time(),
            ?INFO("servers start end, use ~w s", [EndTick - StartTick]);
        _ ->
            io:format("servers starting ~w s~n", [Count]),
            timer:sleep(1000),
            wait_start_end(Count + 1)
    end.

stop() ->
    OnlineMap = lib_role_manage:get_online_map(),
    maps:fold(fun(RoleId, _, _Acc) -> lib_role_login:logout(RoleId) end, ok, OnlineMap),
    ok.


set_server_state(Server, State) ->
    ets:insert(?ETS_SERVER_STATE, {Server, State}),
    case State of
        ?SERVER_STARTED ->
            io:format("server : ~w is started~n", [Server]);
        _ ->
            ignore
    end.


set_start_tick() ->
    erlang:put(?START_TICK, lib_timer:unix_time()).

get_start_tick() ->
    case erlang:get(?START_TICK) of
        undefined -> 0;
        StartTick -> StartTick
    end.