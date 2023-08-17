%% coding: utf-8
-module(lib_counter).
-author("weisenchang").

-include("common.hrl").
-include("mnesia.hrl").

-record(uid, {key = undefined, val = 0}).

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
    Uid = db_mnesia:read(?DB_UID, Key, #uid{key = Key}),
    Val = Uid#uid.val + 1,
    db_mnesia:write(?DB_UID, #key_value{key = Key, value = Uid#uid{val = Val}}),
    Val.