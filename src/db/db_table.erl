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
set_map_value(?MAP, SubType, Name, Value, Map) ->
    List = maps:values(Value),
    set_map_value(?LIST, SubType, Name, List, Map);
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
get_map_value(?MAP, SubType, Name, Map) ->
    Value = get_map_value(?LIST, SubType, Name, Map),
    lists:foldl(fun(V, Acc) -> maps:put(get_key(V), V, Acc) end, #{}, Value);
get_map_value(Type, _, Name, Map) ->
    map_to_record(maps:get(lib_types:to_binary(Name), Map, #{}), Type).

get_key(Record) ->
    Tab = element(1, Record),
    #table{key = KeyName} = get_table(Tab),
    Map = db_table:record_to_map(Record),
    FieldMap = get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap, #field{}),
    get_map_value(Field, Map).


set_field_value(Field, Value) ->
    Field#field{value = Value}.

get_field_value(Key, FieldMap) ->
    #field{value = Value} = maps:get(Key, FieldMap),
    Value.

role_tables() ->
	[role, role_friend, role_cache].

sys_tables() ->
	[uid].

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
get_table('key_value') ->
	#table{key = 'key'};
get_table(_)->
	#table{}.

get_fields(Record) when is_record(Record, 'uid') ->
	#'uid'{'key' = F1, 'id' = F2} = Record,
	[
		#field{name = 'key', type = 'string', sub_type = 'undefined', value = F1},
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F2}
	];
get_fields(Record) when is_record(Record, 'role_cache') ->
	#'role_cache'{'id' = F1, 'name' = F2, 'server_id' = F3, 'server_name' = F4} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'name', type = 'string', sub_type = 'undefined', value = F2},
		#field{name = 'server_id', type = 'int', sub_type = 'undefined', value = F3},
		#field{name = 'server_name', type = 'string', sub_type = 'undefined', value = F4}
	];
get_fields(Record) when is_record(Record, 'role_friend') ->
	#'role_friend'{'id' = F1, 'friend_map' = F2, 'apply_list' = F3, 'black_list' = F4} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'friend_map', type = 'map', sub_type = 'friend', value = F2},
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
	#'role'{'id' = F1, 'name' = F2, 'server_id' = F3, 'server_name' = F4, 'account' = F5, 'item_map' = F6, 'item_list' = F7} = Record,
	[
		#field{name = 'id', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'name', type = 'string', sub_type = 'undefined', value = F2},
		#field{name = 'server_id', type = 'int', sub_type = 'undefined', value = F3},
		#field{name = 'server_name', type = 'string', sub_type = 'undefined', value = F4},
		#field{name = 'account', type = 'string', sub_type = 'undefined', value = F5},
		#field{name = 'item_map', type = 'int', sub_type = 'undefined', value = F6},
		#field{name = 'item_list', type = 'list', sub_type = 'key_value', value = F7}
	];
get_fields(Record) when is_record(Record, 'key_value') ->
	#'key_value'{'key' = F1, 'value' = F2, 'other' = F3} = Record,
	[
		#field{name = 'key', type = 'int', sub_type = 'undefined', value = F1},
		#field{name = 'value', type = 'int', sub_type = 'undefined', value = F2},
		#field{name = 'other', type = 'list', sub_type = 'int', value = F3}
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
		'server_id' => #field{name = 'server_id', type = 'int', sub_type = 'undefined'},
		'server_name' => #field{name = 'server_name', type = 'string', sub_type = 'undefined'}
	};
get_field_map('role_friend') ->
	#{
		'id' => #field{name = 'id', type = 'int', sub_type = 'undefined'},
		'friend_map' => #field{name = 'friend_map', type = 'map', sub_type = 'friend'},
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
		'server_id' => #field{name = 'server_id', type = 'int', sub_type = 'undefined'},
		'server_name' => #field{name = 'server_name', type = 'string', sub_type = 'undefined'},
		'account' => #field{name = 'account', type = 'string', sub_type = 'undefined'},
		'item_map' => #field{name = 'item_map', type = 'int', sub_type = 'undefined'},
		'item_list' => #field{name = 'item_list', type = 'list', sub_type = 'key_value'}
	};
get_field_map('key_value') ->
	#{
		'key' => #field{name = 'key', type = 'int', sub_type = 'undefined'},
		'value' => #field{name = 'value', type = 'int', sub_type = 'undefined'},
		'other' => #field{name = 'other', type = 'list', sub_type = 'int'}
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
		'server_id' = get_field_value('server_id', FieldMap),
		'server_name' = get_field_value('server_name', FieldMap)
	};
field_map_to_record('role_friend', FieldMap) ->
	#'role_friend'{
		'id' = get_field_value('id', FieldMap),
		'friend_map' = get_field_value('friend_map', FieldMap),
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
		'server_id' = get_field_value('server_id', FieldMap),
		'server_name' = get_field_value('server_name', FieldMap),
		'account' = get_field_value('account', FieldMap),
		'item_map' = get_field_value('item_map', FieldMap),
		'item_list' = get_field_value('item_list', FieldMap)
	};
field_map_to_record('key_value', FieldMap) ->
	#'key_value'{
		'key' = get_field_value('key', FieldMap),
		'value' = get_field_value('value', FieldMap),
		'other' = get_field_value('other', FieldMap)
	};
field_map_to_record(_, _FieldMap) ->
	undefined.

