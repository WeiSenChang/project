%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    listen_role_login/0,
    listen_role_logout/0,
    listen_zero_timer/0,
    listen_min_timer/0
]).

listen_role_login() ->
    lib_role_other:update_login(),
    mod_server:async_apply(mod_role_manage:get_pid(),
        fun lib_role_manage:insert_online_role/1, [lib_role:role_id()]),
    ok.

listen_role_logout() ->
    lib_role_other:update_logout(),
    mod_server:async_apply(mod_role_manage:get_pid(),
        fun lib_role_manage:remove_online_role/1, [lib_role:role_id()]),
    ok.

listen_zero_timer() ->
    ?DEBUG("role zero timer ~p", [lib_role:role_id()]),
    ok.

listen_min_timer() ->
    ?DEBUG("role min timer ~p", [lib_role:role_id()]),
    ok.