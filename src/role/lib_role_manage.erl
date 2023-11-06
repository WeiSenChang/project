%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").

-define(ONLINE_MAP, online_map).

%% API
-export([
    load_all_role_cache/0,
    insert_online_role/1,
    remove_online_role/1,
    get_online_map/0,
    set_online_map/1,
    get_role_cache/1
]).

load_all_role_cache() ->
    db_mnesia:load_all_data(?DB_ROLE_CACHE).


insert_online_role(Id) ->
    OnlineMap = get_online_map(),
    set_online_map(maps:put(Id, 1, OnlineMap)).

remove_online_role(Id) ->
    OnlineMap = get_online_map(),
    set_online_map(maps:remove(Id, OnlineMap)).

get_online_map() ->
    case erlang:get(?ONLINE_MAP) of
        undefined -> #{};
        OnlineMap -> OnlineMap
    end.

set_online_map(OnlineMap) ->
    erlang:put(?ONLINE_MAP, OnlineMap).

get_role_cache(Id) ->
    db_mnesia:get_data(?DB_ROLE_CACHE, Id).