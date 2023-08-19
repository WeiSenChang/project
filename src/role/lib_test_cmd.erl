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
    lists:foreach(
        fun(_) ->
            Id = lib_role_login:create(Account),
            lib_role_login:login(Id),
            lib_role:change_role_name(Id, Account ++ integer_to_list(Id)),
            lib_role_login:logout(Id)
        end, lists:seq(1, Num)),
    ?DEBUG("~w", [lib_timer:unix_time()]),
    ok;
role_gm(change_name, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("~w", [lib_timer:unix_time()]),
    lists:foreach(
        fun(Id) ->
            lib_role_login:login(Id),
            lib_role:change_role_name(Id, "wsc" ++ integer_to_list(Id)),
            lib_role_login:logout(Id)
        end, lib_mnesia:all_keys(?DB_ROLE)),
    ?DEBUG("~w", [lib_timer:unix_time()]),
    ok;

role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?WARNING("no role gm: ~w", [Gm]).