%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-ifndef('db_table_HRL').
-define('db_table_HRL', true).

-define(DB_UID, uid).

-define(DB_ROLE_CACHE, role_cache).

-define(DB_ROLE_FRIEND, role_friend).

-define(DB_ROLE, role).


-record(uid, {key = "", id = 0}).

-record(role_cache, {id = 0, name = "", server_id = 0, server_name = "", role_map = #{}}).

-record(role_friend, {id = 0, friend_list = [], apply_list = [], black_list = []}).

-record(role, {id = 0, name = "", server_id = "", server_name = "", account = "", item_map = #{}}).

-record(key_value, {key = 0, value = 0, other = []}).

-endif.
