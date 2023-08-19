%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 8月 2023 19:16
%%%-------------------------------------------------------------------
-module(lib_role_cache).
-author("weisenchang").

-include("common.hrl").
-include("ets.hrl").
-include("role.hrl").

%% API
-export([
    ets_init/0,
    put_role_show/2,
    get_role_show/1,
    put_role_fight/2,
    get_role_fight/1,
    load_role_cache/0,
    save_role_cache/0
]).

-define(SAVE_MAP(Ets), {save_map, Ets}).
-define(CACHE_ETS_LIST, [?ETS_ROLE_SHOW, ?ETS_ROLE_FIGHT]).

ets_init() ->
    ets:new(?ETS_ROLE_SHOW, [{?KEY_POS, #role_show.id}|?ETS_OPTS]),
    ets:new(?ETS_ROLE_FIGHT, [{?KEY_POS, #role_fight.id}|?ETS_OPTS]).

put_role_show(RoleShow, IsSave) ->
    ets:insert(?ETS_ROLE_SHOW, RoleShow),
    put_save_map(?ETS_ROLE_SHOW, RoleShow#role_show.id, IsSave).

get_role_show(Id) ->
    get_role_cache(Id, ?ETS_ROLE_SHOW).

put_role_fight(RoleFight, IsSave) ->
    ets:insert(?ETS_ROLE_FIGHT, RoleFight),
    put_save_map(?ETS_ROLE_FIGHT, RoleFight#role_fight.id, IsSave).

get_role_fight(Id) ->
    get_role_cache(Id, ?ETS_ROLE_FIGHT).

get_role_cache(Id, Ets) ->
    case ets:lookup(Ets, Id) of
        [RoleCache] -> RoleCache;
        _ -> undefined
    end.

load_role_cache() ->
    load_role_cache(?CACHE_ETS_LIST).
load_role_cache([]) ->
    ok;
load_role_cache([Ets|T]) ->
    ?DEBUG("~w", [lib_timer:unix_time()]),
    case get_load_cfg(Ets) of
        {Ets, Ids, LoadFun, PutFun} ->
            Fun = fun(Id) -> PutFun(LoadFun(Id), false) end,
            lists:foreach(Fun, Ids);
        _ -> ?WARNING("not_get_load_cfg, ets: ~w", [Ets])
    end,
    ?DEBUG("~w", [lib_timer:unix_time()]),
    load_role_cache(T).

get_load_cfg(?ETS_ROLE_SHOW) ->
    {?ETS_ROLE_SHOW, db_role:all_role_show_id(),
        fun db_role:load_role_show/1, fun lib_role_cache:put_role_show/2};
get_load_cfg(?ETS_ROLE_FIGHT) ->
    {?ETS_ROLE_FIGHT, db_role:all_role_fight_id(),
        fun db_role:load_role_fight/1, fun lib_role_cache:put_role_fight/2};
get_load_cfg(_Ets) -> undefined.

save_role_cache() ->
    save_role_cache(?CACHE_ETS_LIST).
save_role_cache([]) ->
    ok;
save_role_cache([Ets|T]) ->
    SaveMap = get_save_map(Ets),
    put_save_map(#{}, Ets),
    case get_save_cfg(Ets) of
        {Ets, SaveFun, GetFun} ->
            Fun = fun(Id, _) -> save_role_cache(Id, SaveFun, GetFun) end,
            maps:foreach(Fun, SaveMap);
        _ -> ?WARNING("not_save_load_cfg, ets: ~w", [Ets])
    end,
    save_role_cache(T).

save_role_cache(Id, SaveFun, GetFun) ->
    case GetFun(Id) of undefined -> skip; RoleCache -> SaveFun(RoleCache) end.

get_save_cfg(?ETS_ROLE_SHOW) ->
    {?ETS_ROLE_SHOW, fun db_role:save_role_show/1, fun lib_role_cache:get_role_show/1};
get_save_cfg(?ETS_ROLE_FIGHT) ->
    {?ETS_ROLE_FIGHT, fun db_role:save_role_fight/1, fun lib_role_cache:get_role_fight/1};
get_save_cfg(_Ets) -> undefined.


%%%%%%%%%
get_save_map(Ets) ->
    case erlang:get(?SAVE_MAP(Ets)) of
        undefined -> #{};
        SaveMap -> SaveMap
    end.

put_save_map(SaveMap, Ets) ->
    erlang:put(?SAVE_MAP(Ets), SaveMap).

put_save_map(Ets, Id, true) ->
    SaveMap = get_save_map(Ets),
    put_save_map(maps:put(Id, 1, SaveMap), Ets);
put_save_map(_Ets, _Id, _IsSave) ->
    skip.