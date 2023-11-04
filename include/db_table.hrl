%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-ifndef('db_table_HRL').
-define('db_table_HRL', true).

-define(DB_UID, uid).

-define(DB_ROLE_FRIEND, role_friend).

-define(DB_ROLE, role).


-record(uid, {key = "", id = 0}).

-record(role_friend, {id = 0, friend_list = [], apply_list = [], black_list = []}).

-record(role, {id = 0, name = "", money = 0.0, account = "", item_list = []}).

-record(key_value, {key = 0, value = 0}).

-endif.
