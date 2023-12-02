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
    role_login/1,
    role_logout/1,
    change_name/3
]).

get_data(RoleId) ->
    lib_db:get(?DB_ROLE_SHOW, RoleId).

set_data(RoleShow) ->
    lib_db:set(?DB_ROLE_SHOW, RoleShow#db_role_show.role_id, RoleShow).

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

change_name(RoleId, OldName, Name) ->
    RoleNameMap = lib_cache:get_role_name_map(),
    RoleId = maps:get(OldName, RoleNameMap),
    case maps:is_key(Name, RoleNameMap) of
        false ->
            NewRoleNameMap0 = maps:remove(OldName, RoleNameMap),
            NewRoleNameMap1 = maps:put(Name, RoleId, NewRoleNameMap0),
            lib_cache:set_role_name_map(NewRoleNameMap1),
            change_name_success;
        true ->
            change_name_fail
    end.