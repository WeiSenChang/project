%% -*- coding: utf-8 -*-

-module(db_table).

-include("db_table.hrl").

-include("db.hrl").

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
	[db_role_show, db_count].

get_table(db_role) ->
    #table{key = role_id, secs = 300};
get_table(db_role_friend) ->
    #table{key = role_id, secs = 0};
get_table(db_role_show) ->
    #table{key = role_id, secs = 300};
get_table(db_count) ->
    #table{key = key, secs = 0};
get_table(_)->
	#table{}.

get_fields(Record) when is_record(Record, db_role) ->
	#db_role{role_id = F1, name = F2, level = F3, career = F4, exp = F5, login_tick = F6, logout_tick = F7} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = name, type = string, sub_type = undefined, value = F2},
		#field{name = level, type = int, sub_type = undefined, value = F3},
		#field{name = career, type = int, sub_type = undefined, value = F4},
		#field{name = exp, type = int, sub_type = undefined, value = F5},
		#field{name = login_tick, type = int, sub_type = undefined, value = F6},
		#field{name = logout_tick, type = int, sub_type = undefined, value = F7}
	];
get_fields(Record) when is_record(Record, db_role_friend) ->
	#db_role_friend{role_id = F1, friend_list = F2, apply_list = F3, black_list = F4} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = friend_list, type = list, sub_type = r_friend, value = F2},
		#field{name = apply_list, type = list, sub_type = int, value = F3},
		#field{name = black_list, type = list, sub_type = int, value = F4}
	];
get_fields(Record) when is_record(Record, db_role_show) ->
	#db_role_show{role_id = F1, name = F2, level = F3, career = F4} = Record,
	[
		#field{name = role_id, type = int, sub_type = undefined, value = F1},
		#field{name = name, type = string, sub_type = undefined, value = F2},
		#field{name = level, type = int, sub_type = undefined, value = F3},
		#field{name = career, type = int, sub_type = undefined, value = F4}
	];
get_fields(Record) when is_record(Record, db_count) ->
	#db_count{key = F1, value = F2} = Record,
	[
		#field{name = key, type = string, sub_type = undefined, value = F1},
		#field{name = value, type = int, sub_type = undefined, value = F2}
	];
get_fields(Record) when is_record(Record, r_friend) ->
	#r_friend{role_id = F1, role_name = F2, other = F3} = Record,
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
		login_tick => #field{name = login_tick, type = int, sub_type = undefined},
		logout_tick => #field{name = logout_tick, type = int, sub_type = undefined}
	};
get_field_map(db_role_friend) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		friend_list => #field{name = friend_list, type = list, sub_type = r_friend},
		apply_list => #field{name = apply_list, type = list, sub_type = int},
		black_list => #field{name = black_list, type = list, sub_type = int}
	};
get_field_map(db_role_show) ->
	#{
		role_id => #field{name = role_id, type = int, sub_type = undefined},
		name => #field{name = name, type = string, sub_type = undefined},
		level => #field{name = level, type = int, sub_type = undefined},
		career => #field{name = career, type = int, sub_type = undefined}
	};
get_field_map(db_count) ->
	#{
		key => #field{name = key, type = string, sub_type = undefined},
		value => #field{name = value, type = int, sub_type = undefined}
	};
get_field_map(r_friend) ->
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
		login_tick = get_field_value(login_tick, FieldMap),
		logout_tick = get_field_value(logout_tick, FieldMap)
	};
field_map_to_record(db_role_friend, FieldMap) ->
	#db_role_friend{
		role_id = get_field_value(role_id, FieldMap),
		friend_list = get_field_value(friend_list, FieldMap),
		apply_list = get_field_value(apply_list, FieldMap),
		black_list = get_field_value(black_list, FieldMap)
	};
field_map_to_record(db_role_show, FieldMap) ->
	#db_role_show{
		role_id = get_field_value(role_id, FieldMap),
		name = get_field_value(name, FieldMap),
		level = get_field_value(level, FieldMap),
		career = get_field_value(career, FieldMap)
	};
field_map_to_record(db_count, FieldMap) ->
	#db_count{
		key = get_field_value(key, FieldMap),
		value = get_field_value(value, FieldMap)
	};
field_map_to_record(r_friend, FieldMap) ->
	#r_friend{
		role_id = get_field_value(role_id, FieldMap),
		role_name = get_field_value(role_name, FieldMap),
		other = get_field_value(other, FieldMap)
	};
field_map_to_record(_, _FieldMap) ->
	undefined.

get_field_value(Key, FieldMap) ->
    #field{value = Value} = maps:get(Key, FieldMap),
    Value.
