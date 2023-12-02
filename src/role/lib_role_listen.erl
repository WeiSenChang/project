%% -*- coding: utf-8 -*-

-module(lib_role_listen).

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    listen_login/1,
    listen_logout/1
]).

listen_login(RoleId) ->
    Role = lib_role:get_data(RoleId),
    NewRole = Role#db_role{login_tick = lib_time:unix_time()},
    lib_role:set_data(NewRole),
    gen_server:cast(role_manage_server:get_pid(), {role_login, RoleId}),
    ?DEBUG("role login ~w", [RoleId]),
    ok.

listen_logout(RoleId) ->
    Role = lib_role:get_data(RoleId),
    NewRole = Role#db_role{logout_tick = lib_time:unix_time()},
    lib_role:set_data(NewRole),
    gen_server:cast(role_manage_server:get_pid(), {role_logout, RoleId}),
    ?DEBUG("role logout ~w", [RoleId]),
    ok.