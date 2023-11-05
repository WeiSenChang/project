%% coding: utf-8
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 3:26
%%%-------------------------------------------------------------------
-module(main).
-author("weisenchang").

-include("common.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    application:start(server),
    lib_server:start().

stop() ->
    application:stop(server).