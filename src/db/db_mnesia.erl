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
    load_all_data/1,
    load_data/2,
    save_data/0,
    save_data/3,
    delete_data/2,
    erase_role_cache/1
]).

-export([get_data/2, get_all_data/1, set_data/3]).


init_db() ->
    init_mnesia(),
    mnesia:start(),
    create_tables().

all_keys(Tab) ->
    Field = get_key_field(Tab),
    case mnesia:transaction(fun() -> mnesia:all_keys(Tab) end) of
        {atomic, Keys} ->
            [get_map_value(Field, KeyMap) || KeyMap <- Keys];
        _ ->
            []
    end.

load_all_data(Tab) ->
    case mnesia:transaction(fun() -> mnesia:all_keys(Tab) end) of
        {atomic, Keys} ->
            [do_load_data(Tab, KeyMap) || KeyMap <- Keys];
        _ ->
            []
    end.

load_data(Tab, Key) ->
    KeyMap = key_to_key_map(Tab, Key),
    do_load_data(Tab, KeyMap).

save_data() ->
    SaveMap = get_save_map(),
    set_save_map(#{}),
    maps:foreach(
        fun({Tab, Key}, _) ->
            Data = get_data(Tab, Key),
            save_data(Tab, Key, Data)
        end, SaveMap).

save_data(Tab, Key, Data) ->
    DataMap = record_to_map(Data),
    KeyMap = key_to_key_map(Tab, Key),
    mnesia:transaction(fun() -> mnesia:write({Tab, KeyMap, DataMap}) end).

delete_data(Tab, Key) ->
    KeyMap = key_to_key_map(Tab, Key),
    mnesia:transaction(fun() -> mnesia:delete({Tab, KeyMap}) end).

get_data(Tab, Key) ->
    case ets:lookup(?ETS(Tab), Key) of
        [{Key, DataMap}] ->
            map_to_record(DataMap, Tab);
        _ ->
            undefined
    end.

get_all_data(Tab) ->
    ets:foldr(fun({_, DataMap}, Acc) -> [map_to_record(DataMap, Tab) | Acc] end, [], ?ETS(Tab)).

set_data(Tab, Key, Data) ->
    DataMap = record_to_map(Data),
    OldDataMap = get_data(Tab, Key),
    ets:insert(?ETS(Tab), {Key, DataMap}),
    case OldDataMap =/= DataMap of
        true ->
            SaveMap = get_save_map(),
            set_save_map(maps:put({Tab, Key}, 1, SaveMap));
        _ ->
            ignore
    end.

erase_role_cache(RoleId) ->
    RoleTabs = db_table:role_tables(),
    lists:foreach(fun(Tab) -> ets:delete(?ETS(Tab), RoleId) end, RoleTabs).

%%%%%%%%%%%%%
init_mnesia() ->
    case mnesia:system_info(use_dir) of
        false ->
            mnesia:create_schema([node()]);
        _ ->
            skip
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

do_load_data(Tab, KeyMap) ->
    case mnesia:transaction(fun() -> mnesia:read({Tab, KeyMap}) end) of
        {atomic, [{Tab, KeyMap, DataMap}]} ->
            Key = key_map_to_key(Tab, KeyMap),
            ets:insert(?ETS(Tab), {Key, DataMap});
        _ ->
            ignore
    end.

key_to_key_map(Tab, Key) ->
    Field = get_key_field(Tab),
    set_map_value(Field#field{value = Key}, #{}).

key_map_to_key(Tab, KeyMap) ->
    Field = get_key_field(Tab),
    get_map_value(Field, KeyMap).


record_to_map(Record)->
    Fields = db_table:get_fields(Record),
    lists:foldl(
        fun(Field, Acc) ->
            set_map_value(Field, Acc)
        end, #{}, Fields).

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
    FieldMap = db_table:get_field_map(Name),
    NewFieldMap = map_to_field_map(Map, FieldMap),
    db_table:field_map_to_record(Name, NewFieldMap).
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


get_key_field(Tab) ->
    #table{key = Key} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    maps:get(Key, FieldMap).


get_save_map() ->
    case erlang:get(?SAVE_MAP) of
        undefined -> #{};
        SaveMap -> SaveMap
    end.

set_save_map(SaveMap) ->
    erlang:put(?SAVE_MAP, SaveMap).