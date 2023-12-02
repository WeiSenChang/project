%% -*- coding: utf-8 -*-

-module(lib_login).

-include("common.hrl").
-include("server.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([create/1, login/1, logout/1]).

create(Name) ->
    RoleId = lib_count:get_role_id(),
    Role = #db_role{role_id = RoleId, name = Name},
    lib_role:set_data(Role),
    lib_db:save(?DB_ROLE, RoleId),
    RoleId.

login(RoleId) ->
    PName = role_server:get_p_name(RoleId),
    server_sup:start_child(PName, role_server, transient, [RoleId]),
    ok.

logout(RoleId) ->
    PName = role_server:get_p_name(RoleId),
    server_sup:terminate_child(PName),
    server_sup:delete_child(PName),
    ok.