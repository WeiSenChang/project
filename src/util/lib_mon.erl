%% -*- coding: utf-8 -*-

-module(lib_mon).

-include("common.hrl").

%% API
-export([
    estop/0,
    ememory/0,
    ecpu/0
]).

ememory() ->
    erlang:spawn(fun() -> etop:start([{output, text}, {lines, 15}, {sort, memory}])  end).

ecpu() ->
    erlang:spawn(fun() -> etop:start([{output, text}, {lines, 15}, {sort, runtime}]) end).

estop() ->
    etop:stop().