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
    KeyValue = db_mnesia:read(?DB_ROLE, Id),
    Role = role:key_value_to_role(KeyValue),
    Role#role{id = Id}.

save_role(Role) ->
    KeyValue = role:role_to_key_value(Role),
    db_mnesia:write(?DB_ROLE, KeyValue).

load_player_account(Account) ->
    KeyValue = db_mnesia:read(?DB_PLAYER_ACCOUNT, Account),
    PlayerAccount = role:key_value_to_player_account(KeyValue),
    IdList = maps:get(id_list, KeyValue#key_value.value, []),
    RoleList = [load_role(Id) || Id <- IdList],
    PlayerAccount#player_account{account = Account, role_list = RoleList ++ PlayerAccount#player_account.role_list}.

save_player_account(PlayerAccount) ->
    KeyValue = role:player_account_to_key_value(PlayerAccount),
    db_mnesia:write(?DB_PLAYER_ACCOUNT, KeyValue).

