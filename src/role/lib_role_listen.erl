%% coding: utf-8
-module(lib_role_listen).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    listen_role_login/0,
    listen_role_logout/1,
    listen_zero_timer/0,
    listen_min_timer/0
]).

listen_role_login() ->
    Role = lib_role:get_role(),
    mod_server:async_apply(mod_role_manage:get_pid(),
        fun lib_role_manage:insert_role/1, [Role]),
    ok.

listen_role_logout(RoleId) ->
    mod_server:async_apply(mod_role_manage:get_pid(),
        fun lib_role_manage:remove_role/1, [RoleId]),
    ok.

listen_zero_timer() ->
    ?DEBUG("role zero timer ~p", [lib_role:role_id()]),
    ok.

listen_min_timer() ->
    ?DEBUG("role min timer ~p", [lib_role:role_id()]),
    ok.