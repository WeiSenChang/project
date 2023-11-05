%% coding: utf-8
-module(lib_test_cmd).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    role_gm/1,
    role_gm/2,
    role_gm/3,
    role_gm/4,
    role_gm/5
]).

role_gm(Gm) ->
    role_gm(Gm, 0).
role_gm(Gm, Par1) ->
    role_gm(Gm, Par1, 0).
role_gm(Gm, Par1, Par2) ->
    role_gm(Gm, Par1, Par2, 0).
role_gm(Gm, Par1, Par2, Par3) ->
    role_gm(Gm, Par1, Par2, Par3, 0).
role_gm("create", Account, Num, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    Fun = fun(_) -> lib_role_login:create(Account) end,
    lists:foreach(Fun, lists:seq(1, Num)),
    EndTick = lib_timer:unix_time(),
    ?INFO("create role end, use time ~w s", [EndTick - StarTick]);
role_gm("change_name", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    OnLineMap = mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0),
    ?INFO("~w", [map_size(OnLineMap)]),
    maps:fold(
        fun(Id, _, _) ->
            mod_server:sync_apply(mod_role:get_pid(Id),
                fun lib_role:change_role_name/2, [Id, "wsc" ++ lib_types:to_list(Id)])
        end, ok, OnLineMap),
    EndTick = lib_timer:unix_time(),
    ?INFO("change name end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_login", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    Fun = fun(Id) -> lib_role_login:login(Id) end,
    lists:foreach(Fun, lists:seq(1, 1000)),
    EndTick = lib_timer:unix_time(),
    ?INFO("role login end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_logout", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    OnLineMap = mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0),
    maps:foreach(fun(Id, _) -> lib_role_login:logout(Id) end, OnLineMap),
    EndTick = lib_timer:unix_time(),
    ?INFO("role logout end, use time ~w s", [EndTick - StarTick]);
role_gm("change", _Par1, _Par2, _Par3, _Par4) ->
    OnLineMap = mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0),
    [Id] = lib_common:rand_from_list(maps:keys(OnLineMap), 1),
    Role = mod_server:sync_apply(mod_role:get_pid(Id), fun lib_role:get_role/1, [Id]),
    KeyValue = #key_value{key = Id, value = Id, other = [Id]},
    NewRole = Role#role{item_map = maps:put(KeyValue#key_value.key, KeyValue, Role#role.item_map)},
    mod_server:async_apply(mod_role:get_pid(Id), fun lib_role:set_role/1, [NewRole]);
role_gm("friend", _Par1, _Par2, _Par3, _Par4) ->
    OnLineMap = mod_server:sync_apply(mod_role_manage:get_pid(), fun lib_role_manage:get_online_map/0),
    maps:foreach(
        fun(Id, _) ->
            mod_server:sync_apply(mod_role:get_pid(Id),
                fun lib_role:friend/1, [Id])
        end, OnLineMap);


role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?INFO("no role gm: ~w", [Gm]).