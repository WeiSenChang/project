%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 12月 2023 0:40
%%%-------------------------------------------------------------------
-module(db).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    all_keys/1,

    load_cache/1,
    load_cache/2,

    get_cache/1,
    get_cache/2,
    set_cache/3,

    erase_role_cache/1
]).

all_keys(Tab) ->
    db_mnesia:all_keys(Tab).

load_cache(Tab) ->
    db_mnesia:load_cache(Tab).

load_cache(Tab, Key) ->
    db_mnesia:load_cache(Tab, Key).


get_cache(Tab) ->
    db_cache:get_cache(Tab).

get_cache(Tab, Key) ->
    db_cache:get_cache(Tab, Key).

set_cache(Tab, Key, Cache) ->
    db_cache:set_cache(Tab, Key, Cache).

erase_role_cache(RoleId) ->
    db_cache:erase_role_cache(RoleId).