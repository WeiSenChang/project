%% coding: utf-8
-author("weisenchang").

-ifndef('role_HRL').
-define('role_HRL', true).

-define(ONLINE, 1).
-define(OFFLINE, 0).
-define(ONLINE_MAP, online_map).

-record(role, {
    id = 0,
    name = "",
    sex = 0,
    level = 0,
    career = 0,
    account = ""
}).

-record(role_handle, {
    ets = undefined,
    db = undefined,
    get_func = undefined,
    put_func = undefined,
    load_func = undefined,
    save_func = undefined
}).

-record(player_account, {
    account = "",
    tel = "",
    role_map = #{}
}).

-record(role_other, {
    id = 0,
    login_tick = 0,
    logout_tick = 0,
    is_online = 0
}).

-record(role_show, {
    id = 0,
    name = "",
    sex = 0,
    career = 0,
    level = 0,
    league_id = 0,
    league_name = "",
    force = 0,
    frame = 0,
    title = 0
}).

-record(role_fight, {
    id = 0,
    force = 0
}).

-endif.
