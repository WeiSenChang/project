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

%% API
-export([
    init_db/0,
    all_keys/1,
    get_data/2,
    set_data/1,
    save_data/1,
    load_data/2
]).

all_keys(Tab) ->
    AtomTab = lib_types:to_atom(Tab),
    #table{key = KeyName} = db_table:get_table(AtomTab),
    AtomKeyName = lib_types:to_atom(KeyName),
    FieldMap = db_table:get_field_map(AtomTab),
    Field = maps:get(AtomKeyName, FieldMap),
    Fun = fun() -> mnesia:all_keys(AtomTab) end,
    case mnesia:transaction(Fun) of
        {atomic, Keys} ->
            [db_table:get_map_value(Field, KeyMap) || KeyMap <- Keys];
        _ ->
            []
    end.

get_data(Tab, Key) ->
    AtomTab = lib_types:to_atom(Tab),
    case erlang:get({AtomTab, Key}) of
        undefined ->
            DbData = load_data(AtomTab, Key),
            set_data(DbData),
            DbData;
        Data ->
            Data
    end.

set_data(Data) ->
    Tab = element(1, Data),
    #table{key = KeyName} = db_table:get_table(Tab),
    DataMap = db_table:record_to_map(Data),
    FieldMap = db_table:get_field_map(Tab),
    AtomKeyName = lib_types:to_atom(KeyName),
    Field = maps:get(AtomKeyName, FieldMap),
    Key = db_table:get_map_value(Field, DataMap),
    OldData = erlang:put({Tab, Key}, Data),
    if
        OldData =/= Data andalso OldData =/= undefined ->
            save_data(Data);
        true ->
            skip
    end.

load_data(Tab, Key) ->
    AtomTab = lib_types:to_atom(Tab),
    #table{key = KeyName} = db_table:get_table(AtomTab),
    AtomKeyName = lib_types:to_atom(KeyName),
    FieldMap = db_table:get_field_map(AtomTab),
    Field = maps:get(AtomKeyName, FieldMap),
    NewField = db_table:set_field_value(Field, Key),
    KeyMap = db_table:set_map_value(NewField, #{}),
    Fun = fun() -> mnesia:read({AtomTab, KeyMap}) end,
    case mnesia:transaction(Fun) of
        {atomic, [{Tab, KeyMap, DataMap}]} ->
            db_table:map_to_record(DataMap, AtomTab);
        _ ->
            db_table:map_to_record(KeyMap, AtomTab)
    end.

save_data(Data) ->
    Tab = element(1, Data),
    #table{key = KeyName} = db_table:get_table(Tab),
    DataMap = db_table:record_to_map(Data),
    FieldMap = db_table:get_field_map(Tab),
    AtomKeyName = lib_types:to_atom(KeyName),
    Field = maps:get(AtomKeyName, FieldMap),
    Key = db_table:get_map_value(Field, DataMap),
    NewField = db_table:set_field_value(Field, Key),
    KeyMap = db_table:set_map_value(NewField, #{}),
    Fun = fun() -> mnesia:write({Tab, KeyMap, DataMap}) end,
    case mnesia:transaction(Fun) of
        {atomic,ok} -> ok;
        _Other -> ?DEBUG("~w", [_Other])
    end.


init_db() ->
    init_mnesia(),
    mnesia:start(),
    create_tables().

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
    create_tables(CreateTabL),
    mnesia:wait_for_tables(CreateTabL, 1000).
create_tables([]) ->
    ok;
create_tables([Tab|T]) ->
    mnesia:create_table(Tab, [{disc_only_copies, [node()]}]),
    create_tables(T).