%% coding: utf-8
-module(lib_test_cmd).
-author("weisenchang").

-include("common.hrl").
-include("ets.hrl").
-include("mnesia.hrl").

%% API
-export([
    role_gm/5
]).

role_gm(create, Account, Num, _Par3, _Par4) ->
    ?DEBUG("~w", [lib_timer:unix_time()]),
    Fun = fun(_) -> Id = lib_role_login:create(Account),
        change_name(Id, Account ++ integer_to_list(Id)) end,
    lists:foreach(Fun, lists:seq(1, Num)),
    ?DEBUG("~w", [lib_timer:unix_time()]),
    ok;
role_gm(change_name, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("~w", [lib_timer:unix_time()]),
    Fun = fun(Id) -> change_name(Id, "wsc" ++ integer_to_list(Id)) end,
    lists:foreach(Fun, lib_mnesia:all_keys(?DB_ROLE)),
    ?DEBUG("~w", [lib_timer:unix_time()]),
    ok;

role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?WARNING("no role gm: ~w", [Gm]).

change_name(Id, Name) ->
    lib_role_login:login(Id),
    Pid = mod_role:get_pid(Id),
    Fun = fun lib_role:change_role_name/1,
    Args = [Name],
    mod_server:async_apply(Pid, Fun, Args),
    lib_role_login:logout(Id).