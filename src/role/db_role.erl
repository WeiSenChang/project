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
    save_player_account/1
]).

load_role(Id) ->
    db_mnesia:read(?DB_ROLE, Id, #role{id = Id}).

save_role(Role) ->
    db_mnesia:write(?DB_ROLE, #key_value{key = Role#role.id, value = Role}).

load_player_account(Account) ->
    db_mnesia:read(?DB_PLAYER_ACCOUNT, Account, #player_account{account = Account}).

save_player_account(PlayerAccount) ->
    db_mnesia:write(?DB_PLAYER_ACCOUNT, #key_value{key = PlayerAccount#player_account.account, value = PlayerAccount}).

