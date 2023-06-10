%% coding: utf-8
-module(lib_counter).
-author("weisenchang").

-include("common.hrl").
-include("mnesia.hrl").

-define(COUNTER_ROLE_ID, role_id).
-define(COUNTER_MAIL_ID, mail_id).

%% API
-export([
    get_role_id/0,
    get_mail_id/0
]).

get_role_id() ->
    get_counter_id(?COUNTER_ROLE_ID).

get_mail_id() ->
    get_counter_id(?COUNTER_MAIL_ID).


%%%%%%%%%%%%%%%%%%%%
get_counter_id(Key) ->
    #key_value{value = KvMap} = db_mnesia:read(?DB_UID, Key),
    Uid = maps:get(Key, KvMap, 0),
    Uid1 = Uid + 1,
    KvMap1 = maps:put(Key, Uid1, KvMap),
    db_mnesia:write(?DB_UID, #key_value{key = Key, value = KvMap1}),
    Uid1.