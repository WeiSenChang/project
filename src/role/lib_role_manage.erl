%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    get_data/1,
    set_data/1,
    role_create/1,
    role_login/1,
    role_logout/1,
    role_change_name/3
]).

get_data(RoleId) ->
    lib_db:get(?DB_ROLE_SHOW, RoleId).

set_data(RoleShow) ->
    lib_db:set(?DB_ROLE_SHOW, RoleShow#db_role_show.role_id, RoleShow).

role_create(Name) ->
    RoleNameMap = lib_cache:get_role_name_map(),
    case maps:is_key(Name, RoleNameMap) of
        false ->
            RoleId = lib_count:get_role_id(),
            Role = #db_role{role_id = RoleId, name = Name},
            lib_role:set_data(Role),
            lib_db:save(?DB_ROLE, RoleId),
            NewRoleNameMap = maps:put(Name, RoleId, RoleNameMap),
            lib_cache:set_role_name_map(NewRoleNameMap),
            {ok, RoleId};
        true ->
            {error, role_name_exist}
    end.


role_login(RoleId) ->
    OnLineRoleMap = lib_cache:get_online_role_map(),
    OffLineRoleMap = lib_cache:get_offline_role_map(),
    NewOnLineRoleMap = maps:put(RoleId, 1, OnLineRoleMap),
    NewOffLineRoleMap = maps:remove(RoleId, OffLineRoleMap),
    lib_cache:set_online_role_map(NewOnLineRoleMap),
    lib_cache:set_offline_role_map(NewOffLineRoleMap).

role_logout(RoleId) ->
    OnLineRoleMap = lib_cache:get_online_role_map(),
    OffLineRoleMap = lib_cache:get_offline_role_map(),
    NewOnLineRoleMap = maps:remove(RoleId, OnLineRoleMap),
    NewOffLineRoleMap = maps:put(RoleId, 1, OffLineRoleMap),
    lib_cache:set_online_role_map(NewOnLineRoleMap),
    lib_cache:set_offline_role_map(NewOffLineRoleMap).

role_change_name(RoleId, OldName, Name) ->
    RoleNameMap = lib_cache:get_role_name_map(),
    RoleId = maps:get(OldName, RoleNameMap),
    case maps:is_key(Name, RoleNameMap) of
        false ->
            NewRoleNameMap0 = maps:remove(OldName, RoleNameMap),
            NewRoleNameMap1 = maps:put(Name, RoleId, NewRoleNameMap0),
            lib_cache:set_role_name_map(NewRoleNameMap1),
            role_change_name_success;
        true ->
            role_change_name_fail
    end.