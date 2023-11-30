%% coding: utf-8
-module(lib_role).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    get_role/1,
    set_role/1,
    change_role_name/2
]).

change_role_name(RoleId, Name) ->
    Role = get_role(RoleId),
    NewRole = Role#db_role{name = Name},
    set_role(NewRole),
    lib_role_listen:listen_change_name(RoleId).

get_role(RoleId) ->
    db:get_cache(?DB_ROLE, RoleId).

set_role(Role) ->
    db:set_cache(?DB_ROLE, Role#db_role.role_id, Role).