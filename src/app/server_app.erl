%% -*- coding: utf-8 -*-

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    server_sup:start_link().

stop(_State) ->
    ok.
