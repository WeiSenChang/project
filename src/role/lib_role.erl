%% coding: utf-8
-module(lib_role).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    get_role/1,
    change_role_name/2
]).

get_role(Id) ->
    db_mnesia:get_data(?DB_ROLE, Id).

change_role_name(Id, Name) ->
    Role = get_role(Id),
    NewRole = Role#role{name = Name},
    db_mnesia:set_data(NewRole).