%% -*- coding: utf-8 -*-

-module(lib_login).

-include("common.hrl").
-include("server.hrl").

%% API
-export([create/1, login/1, logout/1]).

create(Name) ->
    ?SERVER_STARTED = lib_cache:get_server_state(?SERVER),
    gen_server:call(role_manage_server:get_pid(), {create_role, Name}).

login(RoleId) ->
    ?SERVER_STARTED = lib_cache:get_server_state(?SERVER),
    logout(RoleId),
    PName = role_server:get_p_name(RoleId),
    server_sup:start_child(PName, role_server, transient, [RoleId]),
    ok.

logout(RoleId) ->
    PName = role_server:get_p_name(RoleId),
    server_sup:terminate_child(PName),
    server_sup:delete_child(PName),
    ok.