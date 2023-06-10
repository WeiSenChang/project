%% coding: utf-8
-module(role).
-author("weisenchang").

-include("common.hrl").
-include("role.hrl").

%% API
-export([
    role_to_key_value/1,
    key_value_to_role/1,
    player_account_to_key_value/1,
    key_value_to_player_account/1
]).

role_to_key_value(Role) ->
    KvMap = #{
        id => Role#role.id,
        name => Role#role.name,
        sex => Role#role.sex,
        career => Role#role.career,
        account => Role#role.account,
        level => Role#role.level
    },
    #key_value{key = Role#role.id, value = KvMap}.

key_value_to_role(KeyValue) ->
    #key_value{value = KvMap} = KeyValue,
    #role{
        id = maps:get(id, KvMap, 0),
        name = maps:get(name, KvMap, ""),
        sex = maps:get(sex, KvMap, 0),
        career = maps:get(career, KvMap, 0),
        account = maps:get(account, KvMap, 0),
        level = maps:get(level, KvMap, 0)
    }.

player_account_to_key_value(PlayerAccount) ->
    KvMap = #{
        account => PlayerAccount#player_account.account,
        role_list => role_list_to_key_value_list(PlayerAccount#player_account.role_list),
        tel => PlayerAccount#player_account.tel
    },
    #key_value{key = PlayerAccount#player_account.account, value = KvMap}.

role_list_to_key_value_list(RoleList) ->
    role_list_to_key_value_list(RoleList, []).
role_list_to_key_value_list([], KeyValueList) ->
    KeyValueList;
role_list_to_key_value_list([Role|T], KeyValueList) ->
    KeyValue = role_to_key_value(Role),
    role_list_to_key_value_list(T, [KeyValue|KeyValueList]).

key_value_to_player_account(KeyValue) ->
    #key_value{value = KvMap} = KeyValue,
    #player_account{
        account = maps:get(account, KvMap, ""),
        role_list = key_value_list_to_role_list(maps:get(role_list, KvMap, [])),
        tel = maps:get(tel, KvMap, "")
    }.

key_value_list_to_role_list(KeyValueList) ->
    key_value_list_to_role_list(KeyValueList, []).
key_value_list_to_role_list([], RoleList) ->
    RoleList;
key_value_list_to_role_list([KeyValue|T], RoleList) ->
    Role = key_value_to_role(KeyValue),
    key_value_list_to_role_list(T, [Role|RoleList]).