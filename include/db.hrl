%% -*- coding: utf-8 -*-

-ifndef('db_HRL').
-define('db_HRL', true).

-define(INT, int).
-define(FLOAT, float).
-define(STRING, string).
-define(LIST, list).

-define(ROLE_TAB, "role").
-define(SYS_TAB, "sys").

-define(SAVE_SECS, 300).

-define(CACHE_SAVE, 1).
-define(CACHE_NO_SAVE, 0).

-record(table, {key, type, secs = 0, fields = []}).
-record(field, {name, type, sub_type, value}).

-endif.