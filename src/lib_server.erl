%% coding: utf-8
-module(lib_server).
-author("weisenchang").

-include("common.hrl").

%% API
-export([start/0]).

start() ->
    start_common(),
    start_single(),
    ok.

start_common() ->
    lib_mnesia:init_db(),
    server_sup:start_child(mod_log, mod_log, transient, []),
    server_sup:start_child(mod_timer, mod_timer, transient, []),
    server_sup:start_child(mod_mnesia, mod_mnesia, transient, []),
    server_sup:start_child(mod_counter, mod_counter, transient, []),
    ok.

start_single() ->
    server_sup:start_child(mod_role_manage, mod_role_manage, transient, []),
    server_sup:start_child(mod_friend, mod_friend, transient, []),
    server_sup:start_child(mod_role_cache, mod_role_cache, transient, []),
    ok.