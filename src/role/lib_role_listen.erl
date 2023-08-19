%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    listen_role_login/0,
    listen_role_logout/0,

    listen_min_timer/0,
    listen_hour_timer/0,
    listen_zero_timer/0
]).

listen_role_login() ->
    lib_role_other:update_login(),
    lib_role_manage:async_insert_online_role(lib_role:role_id()),
    ok.

listen_role_logout() ->
    lib_role_other:update_logout(),
    lib_role_manage:async_remove_online_role(lib_role:role_id()),
    ok.

listen_min_timer() ->
    ?DEBUG("role min timer ~p", [lib_role:role_id()]),
    ok.

listen_hour_timer() ->
    ?DEBUG("role hour timer ~p", [lib_role:role_id()]),
    ok.

listen_zero_timer() ->
    ?DEBUG("role zero timer ~p", [lib_role:role_id()]),
    ok.