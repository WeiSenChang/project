%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-ifndef('db_table_HRL').
-define('db_table_HRL', true).


-include("db.hrl").


-define(DB_UID, db_uid).

-define(DB_ROLE_CACHE, db_role_cache).

-define(DB_ROLE_FRIEND, db_role_friend).

-define(DB_ROLE, db_role).


-record(db_uid, {
	key = 0, 
	id = 0 
}).

-record(db_role_cache, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0, 
	is_online = 0, 
	offline_tick = 0 
}).

-record(db_role_friend, {
	role_id = 0, 
	friend_list = [], % [#'r_friend'{}]
	apply_list = [], % [integer()]
	black_list = [] % [integer()]
}).

-record(r_friend, {
	role_id = 0, 
	role_name = "", 
	other = [] % [string()]
}).

-record(db_role, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0, 
	exp = 0, 
	is_online = 0, 
	offline_tick = 0 
}).

-endif.
