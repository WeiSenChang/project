%% coding: utf-8
-module(lib_role_flag).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    get_ets_cache_flag/1,
    put_ets_cache_flag/2
]).

get_ets_cache_flag(Ets) ->
    get_role_flag(?ROLE_ETS_CACHE(Ets)).

put_ets_cache_flag(Ets, Flag) ->
    put_role_flag(?ROLE_ETS_CACHE(Ets), Flag).

%% 内部接口
%%%%%%%%%%
get_role_flag(Key) ->
    case erlang:get(Key) of
        undefined -> 0;
        Flag -> Flag
    end.

put_role_flag(Key, Flag) ->
    erlang:put(Key, Flag).