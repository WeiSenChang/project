%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    listen_role_login/1,
    listen_role_logout/1,
    listen_change_name/1
]).

listen_role_login(RoleId) ->
    Role = lib_role:get_role(RoleId),
    NewRole = Role#db_role{is_online = ?ONLINE},
    lib_role:set_role(NewRole),
    update_role_cache(RoleId),
    ok.

listen_role_logout(RoleId) ->
    Role = lib_role:get_role(RoleId),
    NewRole = Role#db_role{is_online = ?OFFLINE, offline_tick = lib_timer:unix_time()},
    lib_role:set_role(NewRole),
    update_role_cache(RoleId),
    ok.

listen_change_name(RoleId) ->
    update_role_cache(RoleId),
    ok.

update_role_cache(RoleId) ->
    #db_role{name = Name, level = Level, career = Career, is_online = IsOnline, offline_tick = OffLineTick} = lib_role:get_role(RoleId),
    RoleCache = #db_role_cache{role_id = RoleId, name = Name, level = Level, career = Career, is_online = IsOnline, offline_tick = OffLineTick},
    lib_role_manage:set_role_cache(RoleCache).