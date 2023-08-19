%% coding: utf-8
-module(lib_role_flag).
-author("weisenchang").

-include("common.hrl").

-define(ROLE_SAVE_FLAG(Ets), {role_save_flag, Ets}).
-define(ROLE_SHOW_FLAG, role_show_flag).

%% API
-export([
    get_save_flag/1,
    put_save_flag/2,
    get_show_flag/0,
    put_show_flag/1
]).

get_save_flag(Ets) ->
    get_role_flag(?ROLE_SAVE_FLAG(Ets)).

put_save_flag(Ets, Flag) ->
    put_role_flag(?ROLE_SAVE_FLAG(Ets), Flag).

get_show_flag() ->
    get_role_flag(?ROLE_SHOW_FLAG).

put_show_flag(Flag) ->
    put_role_flag(?ROLE_SHOW_FLAG, Flag).

%% 内部接口
%%%%%%%%%%
get_role_flag(Key) ->
    case erlang:get(Key) of
        undefined -> 0;
        Flag -> Flag
    end.

put_role_flag(Key, Flag) ->
    erlang:put(Key, Flag).