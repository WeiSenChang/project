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
    Id = lib_counter:get_role_id(),
    Role = #role{id = Id, name = Name},
    db_mnesia:save_data(Role),
    Id.

login(Id) ->
    RegName = mod_role:get_process_name(Id),
    server_sup:start_child(RegName, mod_role, transient, [Id]),
    ok.

logout(Id) ->
    mod_server:sync_apply(mod_role:get_pid(Id), fun mod_role:logout/1, [Id]),
    server_sup:terminate_child(mod_role:get_process_name(Id)),
    server_sup:delete_child(mod_role:get_process_name(Id)),
    ok.