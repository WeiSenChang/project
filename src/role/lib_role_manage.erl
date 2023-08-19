%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    insert_online_role/1,
    remove_online_role/1,

    get_online_map/0,
    put_online_map/1,

    i/0
]).

-export([
    loop/0,
    min/0,
    hour/0,
    zero/0
]).

loop() ->
    ?DEBUG("lib_role_mange loop"),
    ok.

min() ->
    ?DEBUG("lib_role_mange min"),
    notify_role_timer(fun lib_role_listen:listen_min_timer/0),
    ok.

hour() ->
    ?DEBUG("lib_role_mange hour"),
    ok.

zero() ->
    ?DEBUG("lib_role_mange zero"),
    notify_role_timer(fun lib_role_listen:listen_zero_timer/0),
    ok.

notify_role_timer(Func) ->
    Fun = fun(Id, _) -> mod_server:async_apply(mod_role:get_pid(Id), Func, []) end,
    maps:foreach(Fun, get_online_map()).

i() ->
    mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0, []).

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