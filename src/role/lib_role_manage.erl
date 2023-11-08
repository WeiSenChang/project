%% coding: utf-8
-module(lib_role_manage).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    load_all_role_cache/0,
    insert_online_role/1,
    remove_online_role/1,
    get_online_map/0,
    set_online_map/1,
    get_role_cache/1,
    set_role_cache/1,
    set_name_id_map/1,
    get_name_id_map/0,
    set_id_name_map/1,
    get_id_name_map/0
]).

-define(ONLINE_MAP, online_map).
-define(NAME_ID_MAP, name_id_map).
-define(ID_NAME_MAP, id_name_map).


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

set_role_cache(RoleCache) ->
    db_mnesia:set_data(RoleCache),
    #role_cache{id = Id, name = Name} = RoleCache,
    update_id_name_map(Id, Name).


update_id_name_map(Id, Name) ->
    NameIdMap = get_name_id_map(),
    IdNameMap = get_id_name_map(),
    OldName = maps:get(Id, IdNameMap, ""),
    NewNameIdMap0 = maps:remove(OldName, NameIdMap),
    NewNameIdMap = maps:put(Name, Id, NewNameIdMap0),
    NewIdNameMap = maps:put(Id, Name, IdNameMap),
    set_name_id_map(NewNameIdMap),
    set_id_name_map(NewIdNameMap).

set_name_id_map(NameMap) ->
    erlang:put(?NAME_ID_MAP, NameMap).

get_name_id_map() ->
    case erlang:get(?NAME_ID_MAP) of
        undefined -> #{};
        NameMap -> NameMap
    end.

set_id_name_map(NameMap) ->
    erlang:put(?ID_NAME_MAP, NameMap).

get_id_name_map() ->
    case erlang:get(?ID_NAME_MAP) of
        undefined -> #{};
        NameMap -> NameMap
    end.