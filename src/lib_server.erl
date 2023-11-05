%% coding: utf-8
-module(lib_server).
-author("weisenchang").

-include("common.hrl").

%% API
-export([start/0]).

start() ->
    StarTick = lib_timer:unix_time(),

    db_mnesia:init_db(),
    server_sup:start_child(mod_timer, mod_timer, transient, []),
    server_sup:start_child(mod_log, mod_log, transient, []),
    server_sup:start_child(mod_counter, mod_counter, transient, []),
    server_sup:start_child(mod_role_manage, mod_role_manage, transient, []),
    server_sup:start_child(mod_friend, mod_friend, transient, []),

    EndTick = lib_timer:unix_time(),
    ?INFO("server start success, use time ~w s", [EndTick - StarTick]),
    ok.