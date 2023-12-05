%% -*- coding: utf-8 -*-

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Ret = server_sup:start_link(),
    lib_server:start(),
    Ret.

stop(_State) ->
    ok.
