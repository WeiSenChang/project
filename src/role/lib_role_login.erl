%% coding: utf-8
-module(lib_role_login).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

%% API
-export([
    create/1,
    login/1,
    logout/1
]).

create(Name) ->
    RoleId = lib_counter:get_role_id(),
    Role = #db_role{role_id = RoleId, name = Name},
    db_mnesia:save_data(?DB_ROLE, RoleId, Role),
    RoleId.

login(RoleId) ->
    RegName = mod_role:get_process_name(RoleId),
    server_sup:start_child(RegName, mod_role, transient, [RoleId]),
    ok.

logout(RoleId) ->
    mod_server:sync_apply(mod_role:get_pid(RoleId), fun mod_role:logout/1, [RoleId]),
    server_sup:terminate_child(mod_role:get_process_name(RoleId)),
    server_sup:delete_child(mod_role:get_process_name(RoleId)),
    db_mnesia:erase_role_cache(RoleId),
    ok.