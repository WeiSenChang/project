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

change_role_name(Id, Name) ->
    Role = get_role(Id),
    NewRole = Role#role{name = Name},
    set_role(NewRole),
    lib_role_listen:listen_change_name(Id).


friend(Id) ->
    Friend = db_mnesia:get_data(?DB_ROLE_FRIEND, Id),
    Value = #friend{key = -1, value = "wsc" ++ lib_types:to_list(-1), other = ["---asdsadasasd"]},
    NewFriend = Friend#role_friend{friend_list = lists:keystore(-1, #friend.key, Friend#role_friend.friend_list, Value)},
    db_mnesia:set_data(NewFriend).


get_role(Id) ->
    db_mnesia:get_data(?DB_ROLE, Id).

set_role(Role) ->
    db_mnesia:set_data(Role).