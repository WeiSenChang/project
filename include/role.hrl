%% coding: utf-8
-author("weisenchang").

-ifndef('role_HRL').
-define('role_HRL', true).

-define(ONLINE_ROLE_LIST, online_role_list).

-define(ROLE_ETS_CACHE(Ets), {role_ets_cache, Ets}).

-record(role, {
    id = 0 :: integer(),
    name = "" :: string(),
    sex = 0 :: integer(),
    level = 0 :: integer(),
    career = 0 :: integer(),
    account = "" :: string()
}).

-record(role_handle, {
    ets = undefined :: atom(),
    db = undefined :: atom(),
    get_func = undefined:: term(),
    put_func = undefined:: term(),
    load_func = undefined:: term(),
    save_func = undefined:: term()
}).

-record(player_account, {
    account = "" :: string() ,
    tel = "" :: string(),
    role_list = [] :: list() %% [#role{}]
}).

-endif.
