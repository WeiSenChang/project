%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    listen_role_login/1,
    listen_role_logout/1,

    listen_min_timer/1,
    listen_hour_timer/1,
    listen_zero_timer/1
]).

listen_role_login(Id) ->
    ?DEBUG("role login ~p", [Id]),
    lib_role_manage:async_insert_online_role(Id),
    ok.

listen_role_logout(Id) ->
    ?DEBUG("role logout ~p", [Id]),
    lib_role_manage:async_remove_online_role(Id),
    ok.

listen_min_timer(Id) ->
    ?DEBUG("role min timer ~p", [Id]),
    ok.

listen_hour_timer(Id) ->
    ?DEBUG("role hour timer ~p", [Id]),
    ok.

listen_zero_timer(Id) ->
    ?DEBUG("role zero timer ~p", [Id]),
    ok.