%% -*- coding: utf-8 -*-

-module(lib_test_cmd).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([role_gm/1, role_gm/2, role_gm/3, role_gm/4, role_gm/5]).

role_gm(Gm) ->
    role_gm(Gm, 0).
role_gm(Gm, Par1) ->
    role_gm(Gm, Par1, 0).
role_gm(Gm, Par1, Par2) ->
    role_gm(Gm, Par1, Par2, 0).
role_gm(Gm, Par1, Par2, Par3) ->
    role_gm(Gm, Par1, Par2, Par3, 0).
role_gm("test_time", Secs = [_ | _], _Par2, _Par3, _Par4) ->
    NewSecs =
        case string:tokens(Secs, ",") of
            [Y,Mon,D,H,Min,S] ->
                NowTick = lib_time:to_unix_time(calendar:now_to_local_time(erlang:timestamp())),
                ToDataTime = {{lib_types:to_integer(Y), lib_types:to_integer(Mon), lib_types:to_integer(D)},
                    {lib_types:to_integer(H), lib_types:to_integer(Min), lib_types:to_integer(S)}},
                ToTick = lib_time:to_unix_time(ToDataTime),
                ToTick - NowTick;
            _ ->
                0
        end,
    lib_cache:set_test_secs(NewSecs),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;
role_gm("test_time", Secs, _Par2, _Par3, _Par4) ->
    lib_cache:set_test_secs(lib_types:to_integer(Secs)),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;

role_gm("create", Num, _, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    ?INFO("create role start"),
    NameMap = lib_cache:get_role_name_map(),
    RoleIds = maps:values(NameMap),
    create(lists:max([0 | RoleIds]), Num, 1),
    EndTick = lib_time:unix_time(),
    ?INFO("create role end, use time ~w s", [EndTick - StarTick]);
role_gm("change_name", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OnLineMap = lib_cache:get_online_role_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    change_name(maps:to_list(OnLineMap)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("change name end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_login", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    all_role_login(lib_db:all_keys(?DB_ROLE)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("role login end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_logout", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OnLineMap = lib_cache:get_online_role_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    all_role_logout(maps:to_list(OnLineMap)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("role logout end, use time ~w s", [EndTick - StarTick]);


role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("no role gm: ~w", [Gm]).


%% 内部函数
create(_Value, Num, Count) when Num =< Count ->
    ok;
create(Value, Num, Count) ->
    Name = "wsc" ++ lib_types:to_list(Value + Count),
    case lib_login:create(Name) of
        {ok, RoleId} ->
            lib_login:login(RoleId);
        _Other ->
            ?DEBUG("role create fail, info: ~w", [_Other])
    end,
    create(Value, Num, Count + 1).

change_name([]) ->
    ok;
change_name([{RoleId, _} | Tail]) ->
    Name = "weisenchang" ++ lib_types:to_list(RoleId),
    gen_server:cast(role_server:get_pid(RoleId), {role_change_name, Name}),
    change_name(Tail).


all_role_login([]) ->
    ok;
all_role_login([RoleId | Tail]) ->
    lib_login:login(RoleId),
    all_role_login(Tail).

all_role_logout([]) ->
    ok;
all_role_logout([{RoleId, _} | Tail]) ->
    lib_login:logout(RoleId),
    all_role_logout(Tail).