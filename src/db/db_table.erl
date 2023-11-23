%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-module(db_table).

-include("common.hrl").
-include("db_table.hrl").

-export([
    role_tables/0,
    sys_tables/0,
    get_table/1,
    record_to_map/1,
    map_to_record/2,
    get_field_map/1,
    set_map_value/2,
    get_map_value/2,
    set_field_value/2,
    get_field_value/2
]).

record_to_map(Record)->
	Fields = get_fields(Record),
	fields_to_map(Fields).
fields_to_map(Fields) ->
    fields_to_map(#{}, Fields).
fields_to_map(Map, []) ->
    Map;
fields_to_map(Map, [Field|T]) ->
    NewMap = set_map_value(Field, Map),
    fields_to_map(NewMap, T).
set_map_value(Field, Map) ->
    #field{name = Name, type = Type, sub_type = SubType, value = Value} = Field,
    set_map_value(Type, SubType, Name, Value, Map).
set_map_value(?INT, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_integer(Value), Map);
set_map_value(?FLOAT, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_float(Value), Map);
set_map_value(?STRING, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_binary(Value), Map);
set_map_value(?LIST, SubType, Name, Value, Map) ->
    case SubType of
        ?INT -> maps:put(lib_types:to_binary(Name), Value, Map);
        ?FLOAT -> maps:put(lib_types:to_binary(Name), Value, Map);
        ?STRING -> maps:put(lib_types:to_binary(Name), [lib_types:to_binary(V) || V <- Value], Map);
        _ -> maps:put(lib_types:to_binary(Name), [record_to_map(V) || V <- Value], Map)
    end;
set_map_value(_, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), record_to_map(Value), Map).

map_to_record(Map, Name) ->
	FieldMap = get_field_map(Name),
	NewFieldMap = map_to_field_map(Map, FieldMap),
	field_map_to_record(Name, NewFieldMap).
map_to_field_map(Map, FieldMap) ->
    maps:fold(
        fun(Name, Field, Acc) ->
            Value = get_map_value(Field, Map),
            NewField = Field#field{value = Value},
            maps:put(Name, NewField, Acc)
        end, #{}, FieldMap).

get_map_value(Field, Map) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    get_map_value(Type, SubType, Name, Map).
get_map_value(?INT, _, Name, Map) ->
    lib_types:to_integer(maps:get(lib_types:to_binary(Name), Map, 0));
get_map_value(?FLOAT, _, Name, Map) ->
    lib_types:to_float(maps:get(lib_types:to_binary(Name), Map, 0.0));
get_map_value(?STRING, _, Name, Map) ->
    lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, ""));
get_map_value(?LIST, SubType, Name, Map) ->
    case SubType of
        ?INT -> [lib_types:to_integer(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        ?FLOAT -> [lib_types:to_float(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        ?STRING -> [lib_types:to_list(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        _ -> [map_to_record(V, SubType) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))]
    end;
get_map_value(Type, _, Name, Map) ->
    map_to_record(maps:get(lib_types:to_binary(Name), Map, #{}), Type).

set_field_value(Field, Value) ->
    Field#field{value = Value}.

get_field_value(Key, FieldMap) ->
    #field{value = Value} = maps:get(Key, FieldMap),
    Value.

role_tables() ->
	[role, role_friend].

sys_tables() ->
	[role_cache, uid].

get_table('uid') ->
	#table{key = 'key'};
get_table('role_cache') ->
	#table{key = 'id'};
get_table('role_friend') ->
	#table{key = 'id'};
get_table('friend') ->
	#table{key = 'key'};
get_table('role') ->
	#table{key = 'id'};
get_table(_)->
	#table{}.

get_fields(Record) when is_record(Record, 'uid') ->
	#'uid'{'key' = F1, 'id' = F2} = Record,
	[
		#field{name = 'key', type = 'string', sub_type = 'undefined', value = F1},
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F2}
	];
get_fields(Record) when is_record(Record, 'role_cache') ->
	#'role_cache'{'id' = F1, 'name' = F2, 'level' = F3, 'career' = F4} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'name', type = 'string', sub_type = 'undefined', value = F2},
		#field{name = 'level', type = 'int', sub_type = 'undefined', value = F3},
		#field{name = 'career', type = 'int', sub_type = 'undefined', value = F4}
	];
get_fields(Record) when is_record(Record, 'role_friend') ->
	#'role_friend'{'id' = F1, 'friend_list' = F2, 'apply_list' = F3, 'black_list' = F4} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'friend_list', type = 'list', sub_type = 'friend', value = F2},
		#field{name = 'apply_list', type = 'list', sub_type = 'int', value = F3},
		#field{name = 'black_list', type = 'list', sub_type = 'int', value = F4}
	];
get_fields(Record) when is_record(Record, 'friend') ->
	#'friend'{'key' = F1, 'value' = F2, 'other' = F3} = Record,
	[
		#field{name = 'key', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'value', type = 'string', sub_type = 'undefined', value = F2},
		#field{name = 'other', type = 'list', sub_type = 'string', value = F3}
	];
get_fields(Record) when is_record(Record, 'role') ->
	#'role'{'id' = F1, 'name' = F2, 'level' = F3, 'career' = F4, 'exp' = F5} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'name', type = 'string', sub_type = 'undefined', value = F2},
		#field{name = 'level', type = 'int', sub_type = 'undefined', value = F3},
		#field{name = 'career', type = 'int', sub_type = 'undefined', value = F4},
		#field{name = 'exp', type = 'int', sub_type = 'undefined', value = F5}
	];
get_fields(_) ->
	[].

get_field_map('uid') ->
	#{
		'key' => #field{name = 'key', type = 'string', sub_type = 'undefined'},
		'id' => #field{name = 'id', type = 'int', sub_type = 'undefined'}
	};
get_field_map('role_cache') ->
	#{
		'id' => #field{name = 'id', type = 'int', sub_type = 'undefined'},
		'name' => #field{name = 'name', type = 'string', sub_type = 'undefined'},
		'level' => #field{name = 'level', type = 'int', sub_type = 'undefined'},
		'career' => #field{name = 'career', type = 'int', sub_type = 'undefined'}
	};
get_field_map('role_friend') ->
	#{
		'id' => #field{name = 'id', type = 'int', sub_type = 'undefined'},
		'friend_list' => #field{name = 'friend_list', type = 'list', sub_type = 'friend'},
		'apply_list' => #field{name = 'apply_list', type = 'list', sub_type = 'int'},
		'black_list' => #field{name = 'black_list', type = 'list', sub_type = 'int'}
	};
get_field_map('friend') ->
	#{
		'key' => #field{name = 'key', type = 'int', sub_type = 'undefined'},
		'value' => #field{name = 'value', type = 'string', sub_type = 'undefined'},
		'other' => #field{name = 'other', type = 'list', sub_type = 'string'}
	};
get_field_map('role') ->
	#{
		'id' => #field{name = 'id', type = 'int', sub_type = 'undefined'},
		'name' => #field{name = 'name', type = 'string', sub_type = 'undefined'},
		'level' => #field{name = 'level', type = 'int', sub_type = 'undefined'},
		'career' => #field{name = 'career', type = 'int', sub_type = 'undefined'},
		'exp' => #field{name = 'exp', type = 'int', sub_type = 'undefined'}
	};
get_field_map(_) ->
	#{}.

field_map_to_record('uid', FieldMap) ->
	#'uid'{
		'key' = get_field_value('key', FieldMap),
		'id' = get_field_value('id', FieldMap)
	};
field_map_to_record('role_cache', FieldMap) ->
	#'role_cache'{
		'id' = get_field_value('id', FieldMap),
		'name' = get_field_value('name', FieldMap),
		'level' = get_field_value('level', FieldMap),
		'career' = get_field_value('career', FieldMap)
	};
field_map_to_record('role_friend', FieldMap) ->
	#'role_friend'{
		'id' = get_field_value('id', FieldMap),
		'friend_list' = get_field_value('friend_list', FieldMap),
		'apply_list' = get_field_value('apply_list', FieldMap),
		'black_list' = get_field_value('black_list', FieldMap)
	};
field_map_to_record('friend', FieldMap) ->
	#'friend'{
		'key' = get_field_value('key', FieldMap),
		'value' = get_field_value('value', FieldMap),
		'other' = get_field_value('other', FieldMap)
	};
field_map_to_record('role', FieldMap) ->
	#'role'{
		'id' = get_field_value('id', FieldMap),
		'name' = get_field_value('name', FieldMap),
		'level' = get_field_value('level', FieldMap),
		'career' = get_field_value('career', FieldMap),
		'exp' = get_field_value('exp', FieldMap)
	};
field_map_to_record(_, _FieldMap) ->
	undefined.

