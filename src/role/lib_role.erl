%% coding: utf-8
-module(lib_role).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    get_role/1,
    set_role/1,
    change_role_name/2,
    friend/1
]).

change_role_name(RoleId, Name) ->
    Role = get_role(RoleId),
    NewRole = Role#db_role{name = Name},
    set_role(NewRole),
    lib_role_listen:listen_change_name(RoleId).


friend(RoleId) ->
    Friend = db_mnesia:get_data(?DB_ROLE_FRIEND, RoleId),
    Value = #r_friend{role_id = -1, role_name = "wsc" ++ lib_types:to_list(-1), other = ["---asdsadasasd"]},
    NewFriend = Friend#db_role_friend{friend_list = lists:keystore(-1, #r_friend.role_id, Friend#db_role_friend.friend_list, Value)},
    db_mnesia:set_data(?DB_ROLE_FRIEND, RoleId, NewFriend).


get_role(RoleId) ->
    db_mnesia:get_data(?DB_ROLE, RoleId).

set_role(Role) ->
    db_mnesia:set_data(?DB_ROLE, Role#db_role.role_id, Role).