%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    listen_role_login/1,
    listen_role_logout/1,
    listen_change_name/1
]).

listen_role_login(Id) ->
    mod_server:async_apply(mod_role_manage:get_pid(), fun lib_role_manage:insert_online_role/1, [Id]),
    ok.

listen_role_logout(Id) ->
    mod_server:async_apply(mod_role_manage:get_pid(), fun lib_role_manage:remove_online_role/1, [Id]),
    ok.

listen_change_name(Id) ->
    #role{name = Name, server_id = ServerId, server_name = ServerName} = lib_role:get_role(Id),
    RoleCache = #role_cache{id = Id, name = Name, server_id = ServerId, server_name = ServerName},
    mod_server:async_apply(mod_role_manage:get_pid(), fun lib_role_manage:set_role_cache/1, [RoleCache]),
    ok.