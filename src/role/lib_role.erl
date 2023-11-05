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

change_role_name(Id, Name) ->
    Role = get_role(Id),
    NewRole = Role#role{name = Name},
    set_role(NewRole).


get_role(Id) ->
    db_mnesia:get_data(?DB_ROLE, Id).

set_role(Role) ->
    db_mnesia:set_data(Role).