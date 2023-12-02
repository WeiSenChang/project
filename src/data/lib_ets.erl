%% -*- coding: utf-8 -*-

-module(lib_ets).

-include("common.hrl").
-include("ets.hrl").

%% API
-export([ets_init/0, set/3, get/3, delete/2]).

ets_init() ->
    ?ETS_GLOBAL_CACHE = ets:new(?ETS_GLOBAL_CACHE, ?ETS_OPTS),
    lib_db:ets_init(),
    ok.

get(Ets, Key, Def) ->
    case ets:lookup(Ets, Key) of
        [{Key, Value}] -> Value;
        _ -> Def
    end.

set(Ets, Key, Value) ->
    ets:insert(Ets, {Key, Value}).

delete(Ets, Key) ->
    ets:delete(Ets, Key).