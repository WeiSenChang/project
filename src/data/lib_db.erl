%% -*- coding: utf-8 -*-

-module(lib_db).

-include("common.hrl").
-include("db_table.hrl").
-include("ets.hrl").
-include("db.hrl").

%% API
-export([db_init/0, ets_init/0]).
-export([find_count/1]).
-export([all_keys/1, load_and_set_cache/1, load_and_set_cache/2]).
-export([save/1, save/2, save/3, get/1, get/2, set/3]).
-export([record_to_bson/1, bson_to_record/2]).

db_init() ->
    create_db(),
    ok = start_db(),
    create_tabs().

ets_init() ->
    Tabs = db_table:role_tabs() ++ db_table:sys_tabs(),
    ets_init(Tabs).
ets_init([]) ->
    ok;
ets_init([Tab | Tail]) ->
    Tab = ets:new(Tab, ?ETS_OPTS),
    ets_init(Tail).

find_count(Key) ->
    Fun = fun() -> do_find_count(Key) end,
    {atomic, Value} = mnesia:transaction(Fun),
    Value.

all_keys(?DB_COUNT) ->
    Fun = fun() -> mnesia:all_keys(?DB_COUNT) end,
    {atomic, AllKeys} = mnesia:transaction(Fun),
    AllKeys;
all_keys(Tab) ->
    Field = get_key_field(Tab),
    Fun = fun() -> mnesia:all_keys(Tab) end,
    {atomic, AllKeys} = mnesia:transaction(Fun),
    [get_bson_value(Field, Bson) || Bson <- AllKeys].

load_and_set_cache(Tab) ->
    Field = get_key_field(Tab),
    Fun0 = fun({_, KeyBson, Bson}, Acc) -> set(Tab, Field, KeyBson, Bson), Acc end,
    Fun1 = fun() -> mnesia:foldl(Fun0, ok, Tab) end,
    {atomic, ok} = mnesia:transaction(Fun1),
    ok.

load_and_set_cache(Tab, Key) ->
    KeyBson = key_to_bson(Tab, Key),
    Fun = fun() -> mnesia:read({Tab, KeyBson}) end,
    case mnesia:transaction(Fun) of
        {atomic, [{Tab, KeyBson, Bson}]} ->
            Value = {?CACHE_NO_SAVE, lib_time:unix_time(), Bson},
            lib_ets:set(Tab, Key, Value);
        {atomic, []} ->
            ignore;
        _ -> throw({error, {db_init_fail, Tab, Key}})
    end,
    ok.

save(Tab) ->
    Fun = fun({Key, Value}, Acc) -> save(Tab, Key, Value), Acc end,
    ok = ets:foldl(Fun, ok, Tab).

save(Tab, Key) ->
    Def = {?CACHE_NO_SAVE, lib_time:unix_time(), #{}},
    Value = lib_ets:get(Tab, Key, Def),
    ok = save(Tab, Key, Value).

save(Tab, Key, {?CACHE_SAVE, _, Bson}) ->
    Value = {?CACHE_NO_SAVE, lib_time:unix_time(), Bson},
    lib_ets:set(Tab, Key, Value),
    KeyBson = key_to_bson(Tab, Key),
    Fun = fun() -> mnesia:write({Tab, KeyBson, Bson}) end,
    {atomic, ok} = mnesia:transaction(Fun),
    ?INFO("save, tab: ~w, key: ~w", [Tab, Key]),
    ok;
save(_Tab, _Key, _Value) ->
    ok.


get(Tab) ->
    Fun = fun({_, {_, _, Bson}}, Acc) -> [bson_to_record(Bson, Tab) | Acc] end,
    ets:foldl(Fun, [], Tab).

get(Tab, Key) ->
    Def = {?CACHE_NO_SAVE, 0, #{}},
    {_State, _Tick, Bson} = lib_ets:get(Tab, Key, Def),
    bson_to_record(Bson, Tab).

set(Tab, Key, Value) ->
    Def = {?CACHE_NO_SAVE, lib_time:unix_time(), #{}},
    {State, Tick, Bson} = lib_ets:get(Tab, Key, Def),
    NewBson = record_to_bson(Value),
    case NewBson =/= Bson of
        true ->
            lib_ets:set(Tab, Key, {?CACHE_SAVE, lib_time:unix_time(), NewBson});
        false ->
            lib_ets:set(Tab, Key, {State, Tick, NewBson})
    end.


%% 内部函数
create_db() ->
    case mnesia:system_info(use_dir) of
        false -> mnesia:create_schema([node()]);
        true -> ignore
    end.

start_db() ->
    mnesia:start().

create_tabs() ->
    HasTabs = mnesia:system_info(tables),
    Tabs = db_table:role_tabs() ++ db_table:sys_tabs(),
    CreateTabs = lists:subtract(Tabs, HasTabs),
    create_tabs(CreateTabs),
    mnesia:wait_for_tables(CreateTabs, infinity).
create_tabs([]) ->
    ok;
create_tabs([Tab | Tail]) ->
    mnesia:create_table(Tab, [{disc_only_copies, [node()]}]),
    create_tabs(Tail).

record_to_bson(Record)->
    Fields = db_table:get_fields(Record),
    record_to_bson(#{}, Fields).
record_to_bson(Bson, []) ->
    Bson;
record_to_bson(Bson, [Field | Tail]) ->
    NewBson = set_bson_value(Field, Bson),
    record_to_bson(NewBson, Tail).

set_bson_value(Field, Bson) ->
    #field{name = Name, type = Type, sub_type = SubType, value = Value} = Field,
    set_bson_value(Type, SubType, Name, Value, Bson).
set_bson_value(?INT, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_integer(Value), Bson);
set_bson_value(?FLOAT, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_float(Value), Bson);
set_bson_value(?STRING, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), lib_types:to_binary(Value), Bson);
set_bson_value(?LIST, SubType, Name, Value, Bson) ->
    case SubType of
        ?INT -> maps:put(lib_types:to_binary(Name), Value, Bson);
        ?FLOAT -> maps:put(lib_types:to_binary(Name), Value, Bson);
        ?STRING -> maps:put(lib_types:to_binary(Name), [lib_types:to_binary(V) || V <- Value], Bson);
        _ -> maps:put(lib_types:to_binary(Name), [record_to_bson(V) || V <- Value], Bson)
    end;
set_bson_value(_, _, Name, Value, Bson) ->
    maps:put(lib_types:to_binary(Name), record_to_bson(Value), Bson).

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

get_bson_value(Field, Bson) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    get_bson_value(Type, SubType, Name, Bson).
get_bson_value(?INT, _, Name, Bson) ->
    lib_types:to_integer(maps:get(lib_types:to_binary(Name), Bson, 0));
get_bson_value(?FLOAT, _, Name, Bson) ->
    lib_types:to_float(maps:get(lib_types:to_binary(Name), Bson, 0.0));
get_bson_value(?STRING, _, Name, Bson) ->
    lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, ""));
get_bson_value(?LIST, SubType, Name, Bson) ->
    case SubType of
        ?INT -> [lib_types:to_integer(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        ?FLOAT -> [lib_types:to_float(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        ?STRING -> [lib_types:to_list(V) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))];
        _ -> [bson_to_record(V, SubType) || V <- lib_types:to_list(maps:get(lib_types:to_binary(Name), Bson, []))]
    end;
get_bson_value(Type, _, Name, Bson) ->
    bson_to_record(maps:get(lib_types:to_binary(Name), Bson, #{}), Type).

key_to_bson(Tab, Key) ->
    Field = get_key_field(Tab),
    set_bson_value(Field#field{value = Key}, #{}).

get_key_field(Tab) ->
    #table{key = Key} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    maps:get(Key, FieldMap).


do_find_count(Key) ->
    Count = case mnesia:read({?DB_COUNT, Key}) of
                [{?DB_COUNT, Key, Count0 = #db_count{}}] -> Count0;
                [] -> #db_count{key = Key}
            end,
    NewCount = Count#db_count{value = Count#db_count.value + 1},
    ok = mnesia:write({?DB_COUNT, Key, NewCount}),
    NewCount#db_count.value.


set(Tab, Field, KeyBson, Bson) ->
    Key = get_bson_value(Field, KeyBson),
    Value = {?CACHE_NO_SAVE, lib_time:unix_time(), Bson},
    lib_ets:set(Tab, Key, Value).