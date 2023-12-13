%% -*- coding: utf-8 -*-

-module(main).

-include("common.hrl").
-include("server.hrl").

%% API
-export([start/0, stop/0]).

start() ->
    ok = application:start(server),
    io:format("game start~n").


stop() ->
    lib_server:stop(),
    ok = application:stop(server),
    io:format("game stop~n"),
    init:stop().