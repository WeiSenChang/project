%% -*- coding: utf-8 -*-

-module(main).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    ok = application:start(server).


stop() ->
    lib_server:stop(),
    ok = application:stop(server),
    init:stop().