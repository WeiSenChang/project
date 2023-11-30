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
role_gm("test_time", Secs, _Par2, _Par3, _Par4) ->
    NewSecs =
        if
            is_list(Secs) ->
                case string:tokens(Secs, ",") of
                    [Y,Mon,D,H,Min,S] ->
                        NowTick = lib_timer:to_unix_time(calendar:now_to_local_time(erlang:timestamp())),
                        ToDataTime = {{lib_types:to_integer(Y), lib_types:to_integer(Mon), lib_types:to_integer(D)},
                            {lib_types:to_integer(H), lib_types:to_integer(Min), lib_types:to_integer(S)}},
                        ToTick = lib_timer:to_unix_time(ToDataTime),
                        ToTick - NowTick
                end;
            true ->
                lib_types:to_integer(Secs)
        end,
    mod_timer:set_secs(NewSecs);

role_gm("create", Num, _, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    lists:foreach(
        fun(_) ->
            RoleId = lib_role_login:create("wsc"),
            lib_role_login:login(RoleId),
            mod_server:async_apply(mod_role:get_pid(RoleId), fun lib_role:change_role_name/2, [RoleId, "wsc" ++ lib_types:to_list(RoleId)])
        end, lists:seq(1, Num)),
    EndTick = lib_timer:unix_time(),
    ?INFO("create role end, use time ~w s", [EndTick - StarTick]);
role_gm("change_name", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    OnLineMap = lib_role_manage:get_online_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    maps:fold(
        fun(RoleId, _, _) ->
            lib_role:change_role_name(RoleId, "weisenchang" ++ lib_types:to_list(RoleId))
        end, ok, OnLineMap),
    EndTick = lib_timer:unix_time(),
    ?DEBUG("change name end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_login", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    Fun = fun(Id) -> lib_role_login:login(Id) end,
    lists:foreach(Fun, db_mnesia:all_keys(?DB_ROLE)),
    EndTick = lib_timer:unix_time(),
    ?DEBUG("role login end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_logout", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_timer:unix_time(),
    OnLineMap = lib_role_manage:get_online_map(),
    maps:fold(fun(Id, _, _) -> lib_role_login:logout(Id) end, ok, OnLineMap),
    EndTick = lib_timer:unix_time(),
    ?DEBUG("role logout end, use time ~w s", [EndTick - StarTick]);


role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("no role gm: ~w", [Gm]).

%%min() ->
%%    OnLineMap = lib_role_manage:get_online_map(),
%%    OffLineMap = lib_role_manage:get_offline_map(),
%%    case map_size(OnLineMap) > 0 of
%%        true ->
%%            [{OnLineRoleId,_}] = lib_common:rand_from_list(maps:to_list(OnLineMap), 1),
%%            mod_server:sync_apply(mod_role:get_pid(OnLineRoleId),
%%                fun lib_role:change_role_name/2, [OnLineRoleId, lib_common:log_tab("wsc")]),
%%            lib_role_login:logout(OnLineRoleId);
%%        _ ->
%%            ignore
%%    end,
%%    case map_size(OffLineMap) > 0 of
%%        true ->
%%            [{OffLineRoleId,_}] = lib_common:rand_from_list(maps:to_list(OffLineMap), 1),
%%            lib_role_login:login(OffLineRoleId);
%%        _ ->
%%            ignore
%%    end.