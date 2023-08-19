%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 8月 2023 7:02
%%%-------------------------------------------------------------------
-module(lib_role_other).
-author("weisenchang").
-include("common.hrl").
-include("role.hrl").
-include("ets.hrl").
-include("mnesia.hrl").

%% API
-export([
    get_role_other/0,
    put_role_other/2,
    role_other_handle/0,
    update_login/0,
    update_logout/0,
    check_show_update/0
]).

put_role_other(RoleOther, IsSave) ->
    #role_handle{ets = Ets} = role_other_handle(),
    erlang:put(Ets, RoleOther),
    lib_role:set_save_flag(IsSave, Ets).

get_role_other() ->
    #role_handle{ets = Ets} = role_other_handle(),
    erlang:get(Ets).

role_other_handle() ->
    #role_handle{
        ets = ?ETS_ROLE_OTHER,
        db = ?DB_ROLE_OTHER,
        get_func = fun lib_role_other:get_role_other/0,
        put_func = fun lib_role_other:put_role_other/2,
        load_func = fun db_role:load_role_other/1,
        save_func = fun db_role:save_role_other/1
    }.

update_login() ->
    RoleOther = get_role_other(),
    NewRoleOther = RoleOther#role_other{
        login_tick = lib_timer:unix_time(),
        is_online = ?ONLINE
    },
    put_role_other(NewRoleOther, true).

update_logout() ->
    RoleOther = get_role_other(),
    NewRoleOther = RoleOther#role_other{
        logout_tick = lib_timer:unix_time(),
        is_online = ?OFFLINE
    },
    put_role_other(NewRoleOther, true),
    check_show_update().

check_show_update() ->
    Flag = lib_role_flag:get_show_flag(),
    check_show_update(Flag).
check_show_update(1) ->
    lib_role_flag:put_show_flag(0),
    update_role_show();
check_show_update(_) ->
    skip.

update_role_show() ->
    RoleShow = make_role_show(),
    Pid = mod_role_cache:get_pid(),
    Fun = fun lib_role_cache:put_role_show/2,
    Args = [RoleShow, true],
    mod_server:async_apply(Pid, Fun, Args).

make_role_show() ->
    #role{
        id = Id,
        name = Name,
        sex = Sex,
        career = Career,
        level = Level
    } = lib_role:get_role(),
    #role_show{
        id = Id,
        name = Name,
        sex = Sex,
        career = Career,
        level = Level
    }.