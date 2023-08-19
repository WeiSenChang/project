%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 8月 2023 9:17
%%%-------------------------------------------------------------------
-module(lib_mon).
-author("weisenchang").

%% API
-export([
    estop/0,
    emem/0,
    ecpu/0
]).

estop() ->
    etop:stop().

emem() ->
    etop:start([{interval, 5}, {lines, 10}, {sort, memory}]).

ecpu() ->
    etop:start([{interval, 5}, {lines, 10}, {sort, runtime}]).