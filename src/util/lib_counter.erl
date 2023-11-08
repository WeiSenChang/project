%% coding: utf-8
-module(lib_counter).
-author("weisenchang").

-include("common.hrl").
-include("db_table.hrl").

-define(COUNTER_ROLE_ID, "role_id").
-define(COUNTER_MAIL_ID, "mail_id").

%% API
-export([
    load_all_uid/0,
    get_role_id/0,
    get_mail_id/0
]).

-export([sync_get_id/1]).

load_all_uid() ->
    db_mnesia:load_all_data(?DB_UID).

%% 获取唯一id接口
%%%%%%%%%%%
get_role_id() ->
    get_counter_id(?COUNTER_ROLE_ID).

get_mail_id() ->
    get_counter_id(?COUNTER_MAIL_ID).


%% 内部接口
%%%%%%%%%%%%%%%%%%%%
get_counter_id(Key) ->
    mod_server:sync_apply(mod_counter:get_pid(),
        fun lib_counter:sync_get_id/1, [Key]).

sync_get_id(Key) ->
    Uid = db_mnesia:get_data(?DB_UID, Key),
    NewId = Uid#uid.id + 1,
    NewUid = Uid#uid{id = NewId},
    db_mnesia:set_data(NewUid),
    NewId.