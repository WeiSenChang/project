%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").
-include("role.hrl").

%% API
-export([
    load_all_role_cache/0,
    get_online_map/0,
    get_offline_map/0,
    get_role_cache/1,
    set_role_cache/1,
    get_name_id_map/0,
    get_id_name_map/0
]).

load_all_role_cache() ->
    db_mnesia:load_all_data(?DB_ROLE_CACHE).

get_online_map() ->
    RoleCaches = db_mnesia:get_all_data(?DB_ROLE_CACHE),
    lists:foldl(
        fun(#db_role_cache{role_id = RoleId, is_online = IsOnLine}, Acc) ->
            case IsOnLine of
                ?ONLINE -> maps:put(RoleId, 1, Acc);
                _ -> Acc
            end
        end, #{}, RoleCaches).

get_offline_map() ->
    RoleCaches = db_mnesia:get_all_data(?DB_ROLE_CACHE),
    lists:foldl(
        fun(#db_role_cache{role_id = RoleId, is_online = IsOnLine, offline_tick = OffLineTick}, Acc) ->
            case IsOnLine of
                ?OFFLINE -> maps:put(RoleId, OffLineTick, Acc);
                _ -> Acc
            end
        end, #{}, RoleCaches).

get_name_id_map() ->
    RoleCaches = db_mnesia:get_all_data(?DB_ROLE_CACHE),
    lists:foldl(
        fun(#db_role_cache{role_id = RoleId, name = Name}, Acc) ->
            maps:put(Name, RoleId, Acc)
        end, #{}, RoleCaches).

get_id_name_map() ->
    RoleCaches = db_mnesia:get_all_data(?DB_ROLE_CACHE),
    lists:foldl(
        fun(#db_role_cache{role_id = RoleId, name = Name}, Acc) ->
            maps:put(RoleId, Name, Acc)
        end, #{}, RoleCaches).

get_role_cache(Id) ->
    db_mnesia:get_data(?DB_ROLE_CACHE, Id).

set_role_cache(RoleCache) ->
    db_mnesia:set_data(?DB_ROLE_CACHE, RoleCache#db_role_cache.role_id, RoleCache).