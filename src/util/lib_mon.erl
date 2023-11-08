%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 11月 2023 21:53
%%%-------------------------------------------------------------------
-module(lib_mon).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    estop/0,
    ememory/0,
    ecpu/0
]).

ememory() ->
    erlang:spawn(fun() -> etop:start([{output, text}, {lines, 10}, {sort, memory}])  end).

ecpu() ->
    erlang:spawn(fun() -> etop:start([{output, text}, {lines, 10}, {sort, runtime}]) end).

estop() ->
    etop:stop().