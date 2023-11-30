%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 11月 2023 23:34
%%%-------------------------------------------------------------------
-module(db_cache).
-author("weisenchang").

-include("common.hrl").
-include("db.hrl").

%% API
-export([
    get_cache/1,
    get_cache/2,
    set_cache/3,
    get_cache_state/2,
    set_cache_state/3,
    erase_role_cache/1
]).

get_cache(Tab) ->
    ets:foldr(fun({_, Bson}, Acc) -> [db_mnesia:bson_to_record(Bson, Tab) | Acc] end, [], ?CACHE(Tab)).

get_cache(Tab, Key) ->
    case ets:lookup(?CACHE(Tab), Key) of
        [{Key, Bson}] -> db_mnesia:bson_to_record(Bson, Tab);
        _ -> undefined
    end.

set_cache(Tab, Key, Cache) ->
    Bson = db_mnesia:record_to_bson(Cache),
    OldCache = get_cache(Tab, Key),
    ets:insert(?CACHE(Tab), {Key, Bson}),
    case OldCache =/= Cache of
        true -> set_cache_state(Tab, Key, ?CACHE_STATE_SAVE);
        _ -> ignore
    end.

get_cache_state(Tab, Key) ->
    case ets:lookup(?CACHE_STATE(Tab), Key) of
        [{Key, State}] -> State;
        _ -> ?CACHE_STATE_NO_SAVE
    end.

set_cache_state(Tab, Key, State) ->
    ets:insert(?CACHE_STATE(Tab), {Key, State}).

erase_role_cache(RoleId) ->
    Tabs = db_table:role_tables(),
    lists:foreach(
        fun(Tab) ->
            db_mnesia:save_cache(Tab, RoleId),
            ets:delete(?CACHE(Tab), RoleId),
            ets:delete(?CACHE_STATE(Tab), RoleId)
        end, Tabs).