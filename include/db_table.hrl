%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-ifndef('db_table_HRL').
-define('db_table_HRL', true).


-include("db.hrl").

-define(DB_DB_ROLE, db_db_role).
-define(DB_DB_ROLE_FRIEND, db_db_role_friend).
-define(DB_DB_ROLE_CACHE, db_db_role_cache).
-define(DB_DB_UID, db_db_uid).

-record(db_db_role, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0, 
	exp = 0, 
	is_online = 0, 
	offline_tick = 0 
}).

-record(db_db_role_friend, {
	role_id = 0, 
	friend_list = [], % [#r_friend{}]
	apply_list = [], % [integer()]
	black_list = [] % [integer()]
}).

-record(db_db_role_cache, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0, 
	is_online = 0, 
	offline_tick = 0 
}).

-record(db_db_uid, {
	key = "", 
	value = 0 
}).

-record(r_friend, {
	role_id = 0, 
	role_name = "", 
	other = [] % [string()]
}).

-endif.
