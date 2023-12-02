%% -*- coding: utf-8 -*-

-module(lib_role).

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([get_data/1, set_data/1]).
-export([gen_role_show/1]).
-export([change_name/2]).

get_data(RoleId) ->
    lib_db:get(?DB_ROLE, RoleId).

set_data(Role) ->
    lib_db:set(?DB_ROLE, Role#db_role.role_id, Role).

gen_role_show(RoleId) ->
    #db_role{
        name = Name,
        level = Level,
        career = Career
    } = get_data(RoleId),
    #db_role_show{
        role_id = RoleId,
        name = Name,
        level = Level,
        career = Career
    }.

change_name(RoleId, Name) ->
    #db_role{name = OldName} = get_data(RoleId),
    case gen_server:call(role_manage_server:get_pid(), {change_name, RoleId, OldName, Name}) of
        change_name_success ->
            #db_role{name = OldName} = Role = get_data(RoleId),
            set_data(Role#db_role{name = Name}),
            RoleShow = gen_role_show(RoleId),
            lib_role_manage:set_data(RoleShow);
        _ ->
            ?WARING("role change name fail, name exist. ~w, ~ts", [RoleId, Name])
    end.