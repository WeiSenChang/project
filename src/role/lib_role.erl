%% coding: utf-8
-module(lib_role).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").
-include("ets.hrl").
-include("mnesia.hrl").

%% API
-export([set_ets_cache/2]).

-export([
    put_role/2,
    get_role/0,
    change_role_name/1,
    change_role_name/2
]).

-export([
    role_id/0,
    role_handle/0
]).

put_role(Role, IsSave) ->
    RoleHandle = role_handle(),
    Ets = RoleHandle#role_handle.ets,
    erlang:put(Ets, Role),
    set_ets_cache(IsSave, Ets).

get_role() ->
    RoleHandle = role_handle(),
    Ets = RoleHandle#role_handle.ets,
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

change_role_name(Id, Name) ->
    mod_server:async_apply(mod_role:get_pid(Id),
        fun lib_role:change_role_name/1, [Name]).
change_role_name(Name) ->
    Role = get_role(),
    NewRole = Role#role{name = Name},
    put_role(NewRole, true).

%%%%%%%%%%%%%
set_ets_cache(IsSave, Ets) ->
    case IsSave of
        true ->
            lib_role_flag:put_ets_cache_flag(Ets, 1);
        _ ->
            ignore
    end.