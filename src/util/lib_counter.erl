%% coding: utf-8
-module(lib_counter).
-author("weisenchang").

-include("common.hrl").
-include("mnesia.hrl").
-include("ets.hrl").

-record(uid, {key = undefined, val = 0}).

-define(SAVE_LIST, save_list).

-define(COUNTER_ROLE_ID, role_id).
-define(COUNTER_MAIL_ID, mail_id).

%% API
-export([
    ets_init/0,
    load_uid_i/0,
    save_uid_i/0,

    get_role_id/0,
    get_mail_id/0
]).

-export([sync_get_counter_id/1]).

%% 数据处理
ets_init() ->
    ets:new(?ETS_UID, [{?KEY_POS, #uid.key}|?ETS_OPTS]).

load_uid_i() ->
    Keys = lib_mnesia:all_keys(?DB_UID),
    Fun = fun(Key) -> load_uid_i(Key) end,
    lists:foreach(Fun, Keys).

load_uid_i(Key) ->
    Uid = lib_mnesia:read(?DB_UID, Key, #uid{key = Key}),
    put_uid_i(Uid).

save_uid_i() ->
    SaveList = get_save_list(),
    put_save_list([]),
    Fun = fun(Uid) -> mod_mnesia:insert(?DB_UID, Uid#uid.key, Uid) end,
    lists:foreach(Fun, SaveList).

%% 获取唯一id接口
%%%%%%%%%%%
get_role_id() ->
    get_counter_id(?COUNTER_ROLE_ID).

get_mail_id() ->
    get_counter_id(?COUNTER_MAIL_ID).


%% 内部接口
%%%%%%%%%%%%%%%%%%%%
get_counter_id(Key) ->
    Pid = mod_counter:get_pid(),
    Fun = fun lib_counter:sync_get_counter_id/1,
    Args = [Key],
    mod_server:sync_apply(Pid, Fun, Args).

sync_get_counter_id(Key) ->
    Uid = get_uid_i(Key),
    Val = Uid#uid.val + 1,
    NewUid = Uid#uid{val = Val},
    put_uid_i(NewUid),
    SaveList = get_save_list(),
    NewSaveList = lists:keystore(Key, #uid.key, SaveList, NewUid),
    put_save_list(NewSaveList),
    Val.

get_uid_i(Key) ->
    case ets:lookup(?ETS_UID, Key) of
        [Uid] -> Uid;
        _ -> #uid{key = Key}
    end.

put_uid_i(Uid) ->
    ets:insert(?ETS_UID, Uid).

get_save_list() ->
    case erlang:get(?SAVE_LIST) of
        undefined -> [];
        SaveList -> SaveList
    end.

put_save_list(SaveList) ->
    erlang:put(?SAVE_LIST, SaveList).