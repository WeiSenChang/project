%% coding: utf-8
-module(db_role).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").
-include("mnesia.hrl").

%% API
-export([
    load_role/1,
    save_role/1,
    load_player_account/1,
    save_player_account/1,
    load_role_other/1,
    save_role_other/1,
    load_role_show/1,
    save_role_show/1,
    load_role_fight/1,
    save_role_fight/1,
    all_role_show_id/0,
    all_role_fight_id/0
]).

load_role(Id) ->
    lib_mnesia:read(?DB_ROLE, Id, #role{id = Id}).

save_role(Role) ->
    mod_mnesia:insert(?DB_ROLE, Role#role.id, Role).

load_player_account(Account) ->
    lib_mnesia:read(?DB_PLAYER_ACCOUNT, Account, #player_account{account = Account}).

save_player_account(PlayerAccount) ->
    mod_mnesia:insert(?DB_PLAYER_ACCOUNT, PlayerAccount#player_account.account, PlayerAccount).

load_role_other(Id) ->
    lib_mnesia:read(?DB_ROLE_OTHER, Id, #role_other{id = Id}).

save_role_other(RoleOther) ->
    mod_mnesia:insert(?DB_ROLE_OTHER, RoleOther#role_other.id, RoleOther).

load_role_show(Id) ->
    lib_mnesia:read(?DB_ROLE_SHOW, Id, #role_show{id = Id}).

save_role_show(RoleShow) ->
    mod_mnesia:insert(?DB_ROLE_SHOW, RoleShow#role_show.id, RoleShow).

load_role_fight(Id) ->
    lib_mnesia:read(?DB_ROLE_FIGHT, Id, #role_fight{id = Id}).

save_role_fight(RoleFight) ->
    mod_mnesia:insert(?DB_ROLE_FIGHT, RoleFight#role_fight.id, RoleFight).

all_role_show_id() ->
    lib_mnesia:all_keys(?DB_ROLE_SHOW).

all_role_fight_id() ->
    lib_mnesia:all_keys(?DB_ROLE_FIGHT).