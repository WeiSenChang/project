%% -*- coding: utf-8 -*-

-ifndef('db_table_HRL').
-define('db_table_HRL', true).

-define(DB_ROLE, db_role).
-define(DB_ROLE_FRIEND, db_role_friend).
-define(DB_ROLE_SHOW, db_role_show).
-define(DB_COUNT, db_count).

-record(db_role, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0, 
	exp = 0, 
	login_tick = 0, 
	logout_tick = 0 
}).

-record(db_role_friend, {
	role_id = 0, 
	friend_list = [], % [#r_friend{}]
	apply_list = [], % [integer()]
	black_list = [] % [integer()]
}).

-record(db_role_show, {
	role_id = 0, 
	name = "", 
	level = 0, 
	career = 0 
}).

-record(db_count, {
	key = "", 
	value = 0 
}).

-record(r_friend, {
	role_id = 0, 
	role_name = "", 
	other = [] % [string()]
}).

-endif.
