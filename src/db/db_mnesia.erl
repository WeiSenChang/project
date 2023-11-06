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
    load_all_data/1,
    load_data/2,
    save_data/0,
    save_data/1,
    delete_data/2
]).

-export([get_data/2, set_data/1]).

all_keys(Tab) ->
    #table{key = KeyName} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap),
    case mnesia:transaction(fun() -> mnesia:all_keys(Tab) end) of
        {atomic, Keys} ->
            [db_table:get_map_value(Field, KeyMap) || KeyMap <- Keys];
        _ ->
            []
    end.

load_all_data(Tab) ->
    case mnesia:transaction(fun() -> mnesia:all_keys(Tab) end) of
        {atomic, Keys} ->
            [load_data_1(Tab, KeyMap) || KeyMap <- Keys];
        _ ->
            []
    end.

load_data(Tab, Key) ->
    #table{key = KeyName} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap),
    KeyMap = key_to_map(Field, Key),
    load_data_1(Tab, KeyMap).

load_data_1(Tab, KeyMap) ->
    case mnesia:transaction(fun() -> mnesia:read({Tab, KeyMap}) end) of
        {atomic, [{Tab, KeyMap, DataMap}]} ->
            db_table:map_to_record(DataMap, Tab);
        _ ->
            db_table:map_to_record(KeyMap, Tab)
    end.

save_data() ->
    SaveMap = get_save_map(),
    set_save_map(#{}),
    maps:foreach(
        fun({Tab, Key}, _) ->
            Data = get_data(Tab, Key),
            save_data(Data)
        end, SaveMap).


save_data(Data) ->
    Tab = element(1, Data),
    #table{key = KeyName} = db_table:get_table(Tab),
    DataMap = db_table:record_to_map(Data),
    FieldMap = db_table:get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap),
    Key = db_table:get_map_value(Field, DataMap),
    KeyMap = key_to_map(Field, Key),
    mnesia:transaction(fun() -> mnesia:write({Tab, KeyMap, DataMap}) end).


delete_data(Tab, Key) ->
    #table{key = KeyName} = db_table:get_table(Tab),
    FieldMap = db_table:get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap),
    KeyMap = key_to_map(Field, Key),
    mnesia:transaction(fun() -> mnesia:delete({Tab, KeyMap}) end).

key_to_map(Field, Key) ->
    NewField = db_table:set_field_value(Field, Key),
    db_table:set_map_value(NewField, #{}).


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

get_data(Tab, Key) ->
    case erlang:get({Tab, Key}) of
        undefined ->
            DbData = load_data(Tab, Key),
            set_data(DbData),
            DbData;
        DataMap ->
            db_table:map_to_record(DataMap, Tab)
    end.

set_data(Data) ->
    Tab = element(1, Data),
    #table{key = KeyName} = db_table:get_table(Tab),
    DataMap = db_table:record_to_map(Data),
    FieldMap = db_table:get_field_map(Tab),
    Field = maps:get(KeyName, FieldMap),
    Key = db_table:get_map_value(Field, DataMap),
    OldData = get_old_data(Tab, Key),
    erlang:put({Tab, Key}, DataMap),
    case OldData =/= Data of
        true ->
            SaveMap = get_save_map(),
            set_save_map(maps:put({Tab, Key}, 1, SaveMap));
        _ ->
            skip
    end.

get_old_data(Tab, Key) ->
    case erlang:get({Tab, Key}) of
        undefined ->
            load_data(Tab, Key);
        DataMap ->
            db_table:map_to_record(DataMap, Tab)
    end.



get_save_map() ->
    case erlang:get(?SAVE_MAP) of
        undefined -> #{};
        SaveMap -> SaveMap
    end.

set_save_map(SaveMap) ->
    erlang:put(?SAVE_MAP, SaveMap).