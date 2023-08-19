%% coding: utf-8
-module(lib_role).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").
-include("ets.hrl").
-include("mnesia.hrl").

%% API
-export([set_save_flag/2]).

-export([
    put_role/2,
    get_role/0,
    change_role_name/1
]).

-export([
    role_id/0,
    role_handle/0
]).

put_role(Role, IsSave) ->
    #role_handle{ets = Ets} = lib_role:role_handle(),
    erlang:put(Ets, Role),
    set_save_flag(IsSave, Ets).

get_role() ->
    #role_handle{ets = Ets} = lib_role:role_handle(),
    erlang:get(Ets).

role_handle() ->
    #role_handle{
        ets = ?ETS_ROLE,
        db = ?DB_ROLE,
        get_func = fun lib_role:get_role/0,
        put_func = fun lib_role:put_role/2,
        load_func = fun db_role:load_role/1,
        save_func = fun db_role:save_role/1
    }.

%%%%%%%%%
role_id() ->
    Role = get_role(),
    Role#role.id.

change_role_name(Name) ->
    Role = get_role(),
    NewRole = Role#role{name = Name},
    put_role(NewRole, true),
    lib_role_flag:put_show_flag(1).

%%%%%%%%%%%%%
set_save_flag(true, Ets) ->
    lib_role_flag:put_save_flag(Ets, 1);
set_save_flag(_IsSave, _Ets) ->
    skip.