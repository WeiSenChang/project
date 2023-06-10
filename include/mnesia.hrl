%% coding: utf-8
-author("weisenchang").

-ifndef('mnesia_HRL').
-define('mnesia_HRL', true).

-define(DB_OPTS, [{record_name, key_value}, {attributes, [key, value]}]).

%% 日志
-define(LOG, log).

%% 全局表
-define(DB_UID, uid).
-define(DB_PLAYER_ACCOUNT, player_account).

%% 玩家表
-define(DB_ROLE, role).

-define(DB_ROLE_TAB_LIST, [
    ?DB_ROLE
]).

-define(DB_GLOBAL_TAB_LIST, [
    ?DB_UID,
    ?DB_PLAYER_ACCOUNT
]).

-endif.
