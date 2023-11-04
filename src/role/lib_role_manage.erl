%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").

-define(ONLINE_MAP, online_map).

%% API
-export([
    i/0,
    min/0,
    hour/0,
    zero/0,
    async_insert_online_role/1,
    async_remove_online_role/1,
    get_online_map/0
]).

-export([
    insert_online_role/1,
    remove_online_role/1
]).

min() ->
    notify_role_timer(fun lib_role_listen:listen_min_timer/1),
    ok.

hour() ->
    notify_role_timer(fun lib_role_listen:listen_hour_timer/1),
    ok.

zero() ->
    notify_role_timer(fun lib_role_listen:listen_zero_timer/1),
    ok.

notify_role_timer(Func) ->
    Fun = fun(Id, _) -> notify_role_timer(Id, Func) end,
    maps:foreach(Fun, get_online_map()).
notify_role_timer(Id, Func) ->
    Pid = mod_role:get_pid(Id),
    mod_server:async_apply(Pid, Func, [Id]).

i() ->
    mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0, []).

async_insert_online_role(Id) ->
    Pid = mod_role_manage:get_pid(),
    Fun = fun lib_role_manage:insert_online_role/1,
    Args = [Id],
    mod_server:async_apply(Pid, Fun, Args).

async_remove_online_role(Id) ->
    Pid = mod_role_manage:get_pid(),
    Fun = fun lib_role_manage:remove_online_role/1,
    Args = [Id],
    mod_server:async_apply(Pid, Fun, Args).

%%%%%%%%
insert_online_role(Id) ->
    OnlineMap = get_online_map(),
    put_online_map(maps:put(Id, 1, OnlineMap)).

remove_online_role(Id) ->
    OnlineMap = get_online_map(),
    put_online_map(maps:remove(Id, OnlineMap)).

get_online_map() ->
    case erlang:get(?ONLINE_MAP) of
        undefined -> #{};
        OnlineMap -> OnlineMap
    end.

put_online_map(OnlineMap) ->
    erlang:put(?ONLINE_MAP, OnlineMap).