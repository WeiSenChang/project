%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2023 11:30
%%%-------------------------------------------------------------------
-module(db_mnesia).
-author("weisenchang").

-include("common.hrl").
-include("db.hrl").

%% API
-export([
    init_db/0,

    all_keys/1,
    load_cache/1,
    load_cache/2,
    save_cache/1,
    save_cache/2,
    save_cache/3,

    record_to_bson/1,
    bson_to_record/2
]).

init_db() ->
    init_mnesia(),
    mnesia:start(),
    create_tables().

all_keys(Tab) ->
    Field = get_key_field(Tab),
    case mnesia:transaction(fun() -> mnesia:all_keys(Tab) end) of
        {atomic, KeyBsons} ->
            [get_bson_value(Field, KeyBson) || KeyBson <- KeyBsons];
        _ ->
            []
    end.

load_cache(Tab) ->
    Name = ?CACHE(Tab),
    Fun =
        fun({_, KeyBson, Bson}, _) ->
            Key = bson_to_key(Tab, KeyBson),
            ets:insert(Name, {Key, Bson})
        end,
    mnesia:transaction(fun() -> mnesia:foldl(Fun, ok, Tab) end).

load_cache(Tab, Key) ->
    KeyBson = key_to_bson(Tab, Key),
    case mnesia:transaction(fun() -> mnesia:read({Tab, KeyBson}) end) of
        {atomic, [{Tab, KeyBson, Bson}]} ->
            ets:insert(?CACHE(Tab), {Key, Bson});
        _ ->
            ignore
    end.

save_cache(Tab) ->
    ets:foldl(fun({Key, State}, _) -> save_cache(Tab, Key, State) end, ok, ?CACHE_STATE(Tab)).

save_cache(Tab, Key) ->
    State = db_cache:get_cache_state(Tab, Key),
    save_cache(Tab, Key, State).

save_cache(Tab, Key, State) ->
    case State of
        ?CACHE_STATE_SAVE ->
            db_cache:set_cache_state(Tab, Key, ?CACHE_STATE_NO_SAVE),
            Cache = db_cache:get_cache(Tab, Key),
            Bson = record_to_bson(Cache),
            KeyBson = key_to_bson(Tab, Key),
            mnesia:transaction(fun() -> mnesia:write({Tab, KeyBson, Bson}) end);
        _ ->
            ignore
    end.

%%%%%%%%%%%%%
record_to_bson(Record)->
    Fields = db_table:get_fields(Record),
    lists:foldl(
        fun(Field, Acc) ->
            set_bson_value(Field, Acc)
        end, #{}, Fields).

set_bson_value(Field, Map) ->
    #field{name = Name, type = Type, sub_type = SubType, value = Value} = Field,
    set_bson_value(Type, SubType, Name, Value, Map).
set_bson_value(?INT, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_integer(Value), Map);
set_bson_value(?FLOAT, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_float(Value), Map);
set_bson_value(?STRING, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_binary(Value), Map);
set_bson_value(?LIST, SubType, Name, Value, Map) ->
    case SubType of
        ?INT -> maps:put(lib_types:to_binary(Name), Value, Map);
        ?FLOAT -> maps:put(lib_types:to_binary(Name), Value, Map);
        ?STRING -> maps:put(lib_types:to_binary(Name), [lib_types:to_binary(V) || V <- Value], Map);
        _ -> maps:put(lib_types:to_binary(Name), [record_to_bson(V) || V <- Value], Map)
    end;
set_bson_value(_, _, Name, Value, Map) ->
    maps:put(lib_types:to_binary(Name), record_to_bson(Value), Map).

bson_to_record(Bson, Tab) ->
    FieldMap = db_table:get_field_map(Tab),
    NewFieldMap = update_field_map(Bson, FieldMap),
    db_table:field_map_to_record(Tab, NewFieldMap).
update_field_map(Bson, FieldMap) ->
    maps:fold(
        fun(Name, Field, Acc) ->
            Value = get_bson_value(Field, Bson),
            NewField = Field#field{value = Value},
            maps:put(Name, NewField, Acc)
        end, #{}, FieldMap).

get_bson_value(Field, Map) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    get_bson_value(Type, SubType, Name, Map).
get_bson_value(?INT, _, Name, Map) ->
    lib_types:to_integer(maps:get(lib_types:to_binary(Name), Map, 0));
get_bson_value(?FLOAT, _, Name, Map) ->
    lib_types:to_float(maps:get(lib_types:to_binary(Name), Map, 0.0));
get_bson_value(?STRING, _, Name, Map) ->
    lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, ""));
get_bson_value(?LIST, SubType, Name, Map) ->
    case SubType of
        ?INT -> [lib_types:to_integer(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        ?FLOAT -> [lib_types:to_float(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        ?STRING -> [lib_types:to_list(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))];
        _ -> [bson_to_record(V, SubType) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, []))]
    end;
get_bson_value(Type, _, Name, Map) ->
    bson_to_record(maps:get(lib_types:to_binary(Name), Map, #{}), Type).



%%%%%%%%
init_mnesia() ->
    case mnesia:system_info(use_dir) of
        false ->
            mnesia:create_schema([node()]);
        _ ->
            ignore
    end.

create_tables() ->
    HasTabL = mnesia:system_info(tables),
    TabL = db_table:role_tables() ++ db_table:sys_tables(),
    CreateTabL = lists:subtract(TabL, HasTabL),
    lists:foreach(
        fun(Tab) ->
            mnesia:create_table(Tab, [{disc_only_copies, [node()]}])
        end, CreateTabL),
    mnesia:wait_for_tables(CreateTabL, 1000).

%%%%%%%%%%
key_to_bson(Tab, Key) ->
    Field = get_key_field(Tab),
    set_bson_value(Field#field{value = Key}, #{}).

bson_to_key(Tab, Bson) ->
    Field = get_key_field(Tab),
    get_bson_value(Field, Bson).

get_key_field(Tab) ->
    #table{key = Key} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    maps:get(Key, FieldMap).