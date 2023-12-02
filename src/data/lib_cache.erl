%% -*- coding: utf-8 -*-

-module(lib_cache).

-include("common.hrl").
-include("server.hrl").
-include("ets.hrl").

%% API
-export([
    get_test_secs/0,
    get_server_state/1,
    get_online_role_map/0,
    get_offline_role_map/0,
    get_role_name_map/0
]).

-export([
    set_test_secs/1,
    set_server_state/2,
    set_online_role_map/1,
    set_offline_role_map/1,
    set_role_name_map/1
]).

-define(CACHE_TEST_SECS, test_secs).
-define(CACHE_SERVER_STATE, server_state).
-define(CACHE_ONLINE_ROLE_MAP, online_role_map).
-define(CACHE_OFFLINE_ROLE_MAP, offline_role_map).
-define(CACHE_ROLE_NAME_MAP, role_name_map).

get_test_secs() ->
    get_cache(?CACHE_TEST_SECS, 0).

set_test_secs(Secs) ->
    set_cache(?CACHE_TEST_SECS, Secs).

get_server_state(Server) ->
    get_cache({?CACHE_SERVER_STATE, Server}, ?SERVER_NO_START).

set_server_state(Server, State) ->
    set_cache({?CACHE_SERVER_STATE, Server}, State).

get_online_role_map() ->
    get_cache(?CACHE_ONLINE_ROLE_MAP, #{}).

set_online_role_map(OnlineRoleMap) ->
    set_cache(?CACHE_ONLINE_ROLE_MAP, OnlineRoleMap).

get_offline_role_map() ->
    get_cache(?CACHE_OFFLINE_ROLE_MAP, #{}).

set_offline_role_map(OfflineRoleMap) ->
    set_cache(?CACHE_OFFLINE_ROLE_MAP, OfflineRoleMap).

get_role_name_map() ->
    get_cache(?CACHE_ROLE_NAME_MAP, #{}).

set_role_name_map(RoleNameMap) ->
    set_cache(?CACHE_ROLE_NAME_MAP, RoleNameMap).


%% 内部函数
get_cache(Key, Def) ->
    lib_ets:get(?ETS_GLOBAL_CACHE, Key, Def).

set_cache(Key, Value) ->
    lib_ets:set(?ETS_GLOBAL_CACHE, Key, Value).