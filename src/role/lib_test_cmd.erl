%% coding: utf-8
-module(lib_test_cmd).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    role_gm/5,
    reset_role/0
]).

reset_role() ->
    RoleList = lib_role_manage:get_role_list(),
    reset_role(RoleList, #{}).
reset_role([], AccountMap) ->
    {RemoveList, RoleList} = maps:fold(
        fun(_Account, [KeyValue|T], {AccRemoveList, AccRoleList}) ->
            {AccRemoveList++T, [KeyValue|AccRoleList]}
        end, {[], []}, AccountMap),
    lib_role_manage:put_role_list(RoleList),
    lists:foreach(fun(KeyValue) -> lib_role_manage:put_account(KeyValue) end, RoleList),
    lists:foreach(fun(#key_value{key = Id}) -> lib_role_login:logout(Id) end, RemoveList);
reset_role([KeyValue|T], AccountMap) ->
    Role = role:key_value_to_role(KeyValue),
    Account = Role#role.account,
    KeyValueList = maps:get(Account, AccountMap, []),
    reset_role(T, maps:put(Account, [KeyValue|KeyValueList], AccountMap)).

role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?WARNING("no role gm: ~w", [Gm]).