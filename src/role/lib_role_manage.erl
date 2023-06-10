%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    insert_role/1,
    remove_role/1,

    get_role_list/0,
    put_role_list/1,

    i/0,

    fetch_role_list/3
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
    notify_role_timer(get_role_list(), Func).
notify_role_timer([], _Func) ->
    ok;
notify_role_timer([#key_value{key = Id}|T], Func) ->
    mod_server:async_apply(mod_role:get_pid(Id), Func, []),
    notify_role_timer(T, Func).

i() ->
    mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_role_list/0, []).

fetch_role_list(Pid, Fun, Args) ->
    RoleList = get_role_list(),
    mod_server:async_apply(Pid, Fun, [RoleList|Args]).

insert_role(Role) ->
    KeyValue = role:role_to_key_value(Role),
    RoleList = get_role_list(),
    NewRoleList = lists:keystore(Role#role.id, #key_value.key, RoleList, KeyValue),
    put_role_list(NewRoleList).

remove_role(Id) ->
    RoleList = get_role_list(),
    NewRoleList = lists:keydelete(Id, #key_value.key, RoleList),
    put_role_list(NewRoleList).

get_role_list() ->
    case erlang:get(?ONLINE_ROLE_LIST) of
        undefined -> [];
        RoleList -> RoleList
    end.

put_role_list(RoleList) ->
    erlang:put(?ONLINE_ROLE_LIST, RoleList).