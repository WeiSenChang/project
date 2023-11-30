%% coding: utf-8
-module(lib_counter).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

-define(COUNTER_ROLE_ID, 1).
-define(COUNTER_MAIL_ID, 2).

%% API
-export([
    get_role_id/0,
    get_mail_id/0
]).

%% 获取唯一id接口
%%%%%%%%%%%
get_role_id() ->
    get_counter_id(?COUNTER_ROLE_ID).

get_mail_id() ->
    get_counter_id(?COUNTER_MAIL_ID).


%% 内部接口
%%%%%%%%%%%%%%%%%%%%
get_counter_id(Key) ->
    Uid =
        case db:get_cache(?DB_UID, Key) of
            #db_uid{} = Uid0 -> Uid0;
            _ -> #db_uid{key = Key}
        end,
    NewId = Uid#db_uid.id + 1,
    NewUid = Uid#db_uid{id = NewId},
    db:set_cache(?DB_UID, Key, NewUid),
    NewId.