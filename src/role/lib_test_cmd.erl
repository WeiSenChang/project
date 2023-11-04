%% coding: utf-8
-module(lib_test_cmd).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    role_gm/5
]).

role_gm(create, Account, Num, _Par3, _Par4) ->
    Fun = fun(_) -> lib_role_login:create(Account) end,
    lists:foreach(Fun, lists:seq(1, Num));
role_gm(change_name, _Par1, _Par2, _Par3, _Par4) ->
    Fun = fun(Id) -> change_name(Id, "wsc" ++ lib_types:to_list(Id)) end,
    lists:foreach(Fun, db_mnesia:all_keys(?DB_ROLE));
role_gm(all_role_login, _Par1, _Par2, _Par3, _Par4) ->
    Fun = fun(Id) -> lib_role_login:login(Id) end,
    lists:foreach(Fun, db_mnesia:all_keys(?DB_ROLE));
role_gm(all_role_logout, _Par1, _Par2, _Par3, _Par4) ->
    Fun = fun(Id) -> lib_role_login:logout(Id) end,
    lists:foreach(Fun, db_mnesia:all_keys(?DB_ROLE));

role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("no role gm: ~w", [Gm]).

change_name(Id, Name) ->
    lib_role_login:login(Id),
    Pid = mod_role:get_pid(Id),
    Fun = fun lib_role:change_role_name/2,
    Args = [Id, Name],
    mod_server:async_apply(Pid, Fun, Args),
    lib_role_login:logout(Id).