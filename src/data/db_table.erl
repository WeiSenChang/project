%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-module(db_table).

-include("common.hrl").
-include("db_table.hrl").

-export([
    role_tabs/0,
    sys_tabs/0,
    get_table/1,
    field_map_to_record/2,
    get_fields/1,
    get_field_map/1
]).

role_tabs() ->
	[db_role, db_role_friend].

sys_tabs() ->
	[db_role_cache, db_uid].

get_table(db_role) ->
    #table{key = role_id, secs = 300};
get_table(db_role_friend) ->
    #table{key = role_id, secs = 0};
get_table(db_role_cache) ->
    #table{key = role_id, secs = 300};
get_table(db_uid) ->
    #table{key = key, secs = 0};
get_table(_)->
	#table{}.

get_fields(Record) when is_record(Record, db_role) ->
	#db_role{role_id = F1, name = F2, level = F3, career = F4, exp = F5, is_online = F6, offline_tick = F7} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = name, type = string, sub_type = undefined, value = F2},
		#field{name = level, type = int, sub_type = undefined, value = F3},
		#field{name = career, type = int, sub_type = undefined, value = F4},
		#field{name = exp, type = int, sub_type = undefined, value = F5},
		#field{name = is_online, type = int, sub_type = undefined, value = F6},
		#field{name = offline_tick, type = int, sub_type = undefined, value = F7}
	];
get_fields(Record) when is_record(Record, db_role_friend) ->
	#db_role_friend{role_id = F1, friend_list = F2, apply_list = F3, black_list = F4} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = friend_list, type = list, sub_type = friend, value = F2},
		#field{name = apply_list, type = list, sub_type = int, value = F3},
		#field{name = black_list, type = list, sub_type = int, value = F4}
	];
get_fields(Record) when is_record(Record, db_role_cache) ->
	#db_role_cache{role_id = F1, name = F2, level = F3, career = F4, is_online = F5, offline_tick = F6} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = name, type = string, sub_type = undefined, value = F2},
		#field{name = level, type = int, sub_type = undefined, value = F3},
		#field{name = career, type = int, sub_type = undefined, value = F4},
		#field{name = is_online, type = int, sub_type = undefined, value = F5},
		#field{name = offline_tick, type = int, sub_type = undefined, value = F6}
	];
get_fields(Record) when is_record(Record, db_uid) ->
	#db_uid{key = F1, value = F2} = Record,
	[
		#field{name = key, type = string, sub_type = undefined, value = F1},
		#field{name = value, type = int, sub_type = undefined, value = F2}
	];
get_fields(Record) when is_record(Record, friend) ->
	#friend{role_id = F1, role_name = F2, other = F3} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = role_name, type = string, sub_type = undefined, value = F2},
		#field{name = other, type = list, sub_type = string, value = F3}
	];
get_fields(_) ->
	[].

get_field_map(db_role) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		name => #field{name = name, type = string, sub_type = undefined},
		level => #field{name = level, type = int, sub_type = undefined},
		career => #field{name = career, type = int, sub_type = undefined},
		exp => #field{name = exp, type = int, sub_type = undefined},
		is_online => #field{name = is_online, type = int, sub_type = undefined},
		offline_tick => #field{name = offline_tick, type = int, sub_type = undefined}
	};
get_field_map(db_role_friend) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		friend_list => #field{name = friend_list, type = list, sub_type = friend},
		apply_list => #field{name = apply_list, type = list, sub_type = int},
		black_list => #field{name = black_list, type = list, sub_type = int}
	};
get_field_map(db_role_cache) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		name => #field{name = name, type = string, sub_type = undefined},
		level => #field{name = level, type = int, sub_type = undefined},
		career => #field{name = career, type = int, sub_type = undefined},
		is_online => #field{name = is_online, type = int, sub_type = undefined},
		offline_tick => #field{name = offline_tick, type = int, sub_type = undefined}
	};
get_field_map(db_uid) ->
	#{
		key => #field{name = key, type = string, sub_type = undefined},
		value => #field{name = value, type = int, sub_type = undefined}
	};
get_field_map(friend) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		role_name => #field{name = role_name, type = string, sub_type = undefined},
		other => #field{name = other, type = list, sub_type = string}
	};
get_field_map(_) ->
	#{}.

field_map_to_record(db_role, FieldMap) ->
	#db_role{
		role_id = get_field_value(role_id, FieldMap),
		name = get_field_value(name, FieldMap),
		level = get_field_value(level, FieldMap),
		career = get_field_value(career, FieldMap),
		exp = get_field_value(exp, FieldMap),
		is_online = get_field_value(is_online, FieldMap),
		offline_tick = get_field_value(offline_tick, FieldMap)
	};
field_map_to_record(db_role_friend, FieldMap) ->
	#db_role_friend{
		role_id = get_field_value(role_id, FieldMap),
		friend_list = get_field_value(friend_list, FieldMap),
		apply_list = get_field_value(apply_list, FieldMap),
		black_list = get_field_value(black_list, FieldMap)
	};
field_map_to_record(db_role_cache, FieldMap) ->
	#db_role_cache{
		role_id = get_field_value(role_id, FieldMap),
		name = get_field_value(name, FieldMap),
		level = get_field_value(level, FieldMap),
		career = get_field_value(career, FieldMap),
		is_online = get_field_value(is_online, FieldMap),
		offline_tick = get_field_value(offline_tick, FieldMap)
	};
field_map_to_record(db_uid, FieldMap) ->
	#db_uid{
		key = get_field_value(key, FieldMap),
		value = get_field_value(value, FieldMap)
	};
field_map_to_record(friend, FieldMap) ->
	#friend{
		role_id = get_field_value(role_id, FieldMap),
		role_name = get_field_value(role_name, FieldMap),
		other = get_field_value(other, FieldMap)
	};
field_map_to_record(_, _FieldMap) ->
	undefined.

get_field_value(Key, FieldMap) ->
    #field{value = Value} = maps:get(Key, FieldMap),
    Value.
