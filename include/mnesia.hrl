%% coding: utf-8
-author("weisenchang").

-ifndef('mnesia_HRL').
-define('mnesia_HRL', true).

%% 日志
-define(LOG, log).

%% 全局表
-define(DB_UID, uid).
-define(DB_PLAYER_ACCOUNT, player_account).

%% 玩家表
-define(DB_ROLE, role).
-define(DB_ROLE_OTHER, role_other).
-define(DB_ROLE_FRIEND, role_friend).
-define(DB_ROLE_SHOW, role_show).
-define(DB_ROLE_FIGHT, role_fight).

-define(DB_ROLE_TAB_LIST, [
    ?DB_ROLE,
    ?DB_ROLE_OTHER,
    ?DB_ROLE_FRIEND,
    ?DB_ROLE_SHOW,
    ?DB_ROLE_FIGHT
]).

-define(DB_GLOBAL_TAB_LIST, [
    ?DB_UID,
    ?DB_PLAYER_ACCOUNT
]).

-endif.
