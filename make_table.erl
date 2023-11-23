%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2023 6:47
%%%-------------------------------------------------------------------
-module(make_table).
-author("weisenchang").

-include("./include/game_table.hrl").

-define(MOD, "db_table").
-define(ERL_DIR, "./src/db/").
-define(HRL_DIR, "./include/").

main(_) ->
    create_hrl(),
    create_erl(),
    ok.

create_erl() ->
    Header = gen_erl_header(),
    Body = gen_erl_body(),
    file:write_file(?ERL_DIR ++ ?MOD ++ ".erl", unicode:characters_to_binary(Header ++ Body, utf8)),
    ok.

gen_erl_header() ->
    "%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-module(" ++ ?MOD ++ ").

-include(\"common.hrl\").
-include(\"" ++ ?MOD ++ ".hrl\").

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
    lib_types:to_list(maps:get(lib_types:to_binary(Name), Map, \"\"));
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
    Value.\n\n".


gen_erl_body() ->
    {_, Str1, Str2, Str3, Str4, RoleTabs, SysTabs} = lists:foldr(
        fun(Name, {Acc0, Acc1, Acc2, Acc3, Acc4, Acc5, Acc6}) ->
            #table{key = Key, type = Type, fields = Fields} = table(Name),
            {NewAcc5, NewAcc6} = gen_tabs_str(Type, Name, Acc5, Acc6),
            {AddAcc1, AddAcc2, AddAcc3, AddAcc4, NewAcc0} = gen_fields_erl(Fields, Name, Key, Acc0),
            {NewAcc0, Acc1 ++ AddAcc1, Acc2 ++ AddAcc2, Acc3 ++ AddAcc3, Acc4 ++ AddAcc4, NewAcc5, NewAcc6}
        end, {#{}, "", "", "", "", [], []}, tables()),
    NewStr1 = Str1 ++ "get_table(_)->\n\t#table{}.\n\n",
    NewStr2 = Str2 ++ "get_fields(_) ->\n\t[].\n\n",
    NewStr3 = Str3 ++ "get_field_map(_) ->\n\t#{}.\n\n",
    NewStr4 = Str4 ++ "field_map_to_record(_, _FieldMap) ->\n\tundefined.\n\n",
    RoleTabStr = gen_tabs_str(RoleTabs),
    SysTabStr = gen_tabs_str(SysTabs),
    NewStr5 = "role_tables() ->\n\t[" ++ RoleTabStr ++ "].\n\n",
    NewStr6 = "sys_tables() ->\n\t[" ++ SysTabStr ++ "].\n\n",
    NewStr5 ++ NewStr6 ++ NewStr1 ++ NewStr2 ++ NewStr3 ++ NewStr4.

gen_tabs_str(Tabs) ->
    Len = length(Tabs),
    {TabsStr, _} = lists:foldl(
        fun(TabName, {Acc0, Acc1}) ->
            Str = if Acc1 >= Len -> ""; true -> ", " end,
            {Acc0 ++ TabName ++ Str, Acc1 + 1}
        end, {"", 1}, Tabs),
    TabsStr.
gen_tabs_str(?TAB_TYPE_ROLE, TabName, Acc5, Acc6) ->
    {[TabName|Acc5], Acc6};
gen_tabs_str(?TAB_TYPE_SYS, TabName, Acc5, Acc6) ->
    {Acc5, [TabName|Acc6]};
gen_tabs_str(_, _, Acc5, Acc6) ->
    {Acc5, Acc6}.

gen_fields_erl([], _Name, _Key, Map) ->
    {"", "", "", "", Map};
gen_fields_erl(Fields, Name, Key, Map) ->
    KeyStr = ?TRY_CATCH(atom_to_list(Key), Key),
    Str0 = "get_table('" ++ Name ++ "') ->\n\t#table{key = '" ++ KeyStr ++ "'};\n",
    Str1 = "get_fields(Record) when is_record(Record, '" ++ Name ++ "') ->\n",
    Str2 = "\t#'" ++ Name ++ "'{",
    Str3 = "\t[\n",
    Str4 = "get_field_map('" ++ Name ++ "') ->\n\t#{\n",
    Str5 = "field_map_to_record('" ++ Name ++ "', FieldMap) ->\n\t#'" ++ Name ++ "'{\n",
    {AddMap, NewStr2, NewStr3, NewStr4, NewStr5} = gen_fields_erl(#{}, Str2, Str3, Str4, Str5, 1, length(Fields), Fields),
    Str = Str1 ++ NewStr2 ++ NewStr3,
    {AddStr0, AddStr, AddStr4, AddStr5, NewMap} = maps:fold(
        fun(AddName, _, {AccTabStr, AccAddStr, AccStr4, AccStr5, AccMap}) ->
            case maps:is_key(AddName, AccMap) of
                false ->
                    NewAccMap0 = maps:put(AddName, 1, AccMap),
                    #table{fields = AddFields, key = AddKey} = table(AddName),
                    {AddTabStr, AddAccAddStr, AddAccStr4, AddAccStr5, NewAccMap1} = gen_fields_erl(AddFields, AddName, AddKey, NewAccMap0),
                    {AccTabStr ++ AddTabStr, AccAddStr ++ AddAccAddStr, AccStr4 ++ AddAccStr4, AccStr5 ++ AddAccStr5, NewAccMap1};
                _ ->
                    {AccTabStr, AccAddStr, AccStr4, AccStr5, AccMap}
            end
        end, {"", "", "", "", Map}, AddMap),
    {Str0 ++ AddStr0, Str ++ AddStr, NewStr4 ++ AddStr4, NewStr5 ++ AddStr5, NewMap}.

gen_fields_erl(AddMap, Str1, Str2, Str3, Str4, _, _, []) ->
    {AddMap, Str1, Str2, Str3, Str4};
gen_fields_erl(AddMap, Str1, Str2, Str3, Str4, Index, Len, [Field|T]) ->
    {NewAddMap, AddStr1, AddStr2, AddStr3, AddStr4} = gen_field_erl(Field, Index, Len, AddMap),
    gen_fields_erl(NewAddMap, Str1 ++ AddStr1, Str2 ++ AddStr2, Str3 ++ AddStr3, Str4 ++ AddStr4, Index + 1, Len, T).

gen_field_erl(Field, Index, Len, Map) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    IndexStr = integer_to_list(Index),
    {NewMap, SubTypeStr} = update_record_map(Type, SubType, Map),
    TypeStr = atom_to_list(Type),
    ReadStr = "'" ++ Name ++ "' = F" ++ IndexStr,
    BodyStr = "\t\t#field{name = '" ++ Name ++ "', type = '" ++ TypeStr ++ "', sub_type = '" ++ SubTypeStr ++ "', value = F"++ IndexStr ++ "}",
    MapStr = "\t\t'" ++ Name ++ "' => #field{name = '" ++ Name ++ "', type = '" ++ TypeStr ++ "', sub_type = '" ++ SubTypeStr ++ "'}",
    ToStr = "\t\t'" ++ Name ++ "' = get_field_value('" ++ Name ++ "', FieldMap)",
    {TReadStr, TBodyStr, TMapStr, TToStr} =
        case Index >= Len of
            true ->
                {"} = Record,\n", "\n\t];\n", "\n\t};\n", "\n\t};\n"};
            _ ->
                {", ", ",\n", ",\n", ",\n"}
        end,
    {NewMap, ReadStr ++ TReadStr, BodyStr ++ TBodyStr, MapStr ++ TMapStr, ToStr ++ TToStr}.

update_record_map(?INT, _, Map) ->
    {Map, "undefined"};
update_record_map(?FLOAT, _, Map) ->
    {Map, "undefined"};
update_record_map(?STRING, _, Map) ->
    {Map, "undefined"};
update_record_map(?LIST, SubType, Map) ->
    case SubType of
        ?INT -> {Map, "int"};
        ?FLOAT -> {Map, "float"};
        ?STRING -> {Map, "string"};
        _ -> {maps:put(SubType, 1, Map), SubType}
    end;
update_record_map(Type, _, Map) ->
    {maps:put(Type, 1, Map), Type}.


%%
create_hrl() ->
    Header = gen_hrl_header(),
    {Body0, Body1, _} =
        lists:foldr(
            fun(TabName, {Acc0, Acc1, Acc2}) ->
                #table{def = DefName, fields = Fields} = table(TabName),
                Str0 = "-define(" ++ string:to_upper(DefName) ++ ", " ++ TabName ++ ").\n\n",
                {Str1, NewAcc2} = gen_hrl_body(TabName, Fields, Acc2),
                {Acc0 ++ Str0, Acc1 ++ Str1, NewAcc2}
            end, {"", "", #{}}, tables()),
    Str = Header ++ Body0 ++ Body1 ++ "\n-endif.\n",
    file:write_file(?HRL_DIR ++ ?MOD ++ ".hrl", unicode:characters_to_binary(Str, utf8)).

gen_hrl_header() ->
    "%% -*- coding: utf-8 -*-
%% 数据表定义, 自动创建
-ifndef('" ++ ?MOD ++ "_HRL').
-define('" ++ ?MOD ++ "_HRL', true).\n\n".

%%
gen_hrl_body(_Name, [], Map) ->
    {"", Map};
gen_hrl_body(Name, Fields, Map) ->
    Str0 = "\n-record(" ++ Name ++ ", {\n",
    {AddMap, Str1} = gen_hrl_body(#{}, "", 1, length(Fields), Fields),
    Str2 = Str0 ++ Str1 ++ "}).\n",
    {Str3, NewMap} = maps:fold(
        fun(AddName, _, {AccStr, AccMap}) ->
            case maps:is_key(AddName, AccMap) of
                false ->
                    NewAccMap0 = maps:put(AddName, 1, AccMap),
                    #table{fields = AddFields} = table(AddName),
                    {Str, NewAccMap1} = gen_hrl_body(AddName, AddFields, NewAccMap0),
                    {Str ++ AccStr, NewAccMap1};
                _ ->
                    {AccStr, AccMap}
            end
        end, {"", Map}, AddMap),
    {Str2 ++ Str3, NewMap}.
gen_hrl_body(AddMap, Str, _, _, []) ->
    {AddMap, Str};
gen_hrl_body(AddMap, Str, Index, Len, [Field|T]) ->
    {NewAddMap, FieldStr, CommentStr} = gen_hrl_field(Field, AddMap),
    SplitStr = if Index >= Len -> " "; true -> ", " end,
    gen_hrl_body(NewAddMap, Str ++ FieldStr ++ SplitStr ++ CommentStr, Index + 1, Len, T).

%%
gen_hrl_field(#field{name = Name, type = ?INT}, Map) ->
    {Map, "\t" ++ Name ++ " = 0", "\n"};
gen_hrl_field(#field{name = Name, type = ?FLOAT}, Map) ->
    {Map, "\t" ++ Name ++ " = 0.0", "\n"};
gen_hrl_field(#field{name = Name, type = ?STRING}, Map) ->
    {Map, "\t" ++ Name ++ " = \"\"", "\n"};
gen_hrl_field(#field{name = Name, type = ?LIST, sub_type = SubType}, Map) ->
    Str = "\t" ++ Name ++ " = []",
    case SubType of
        ?INT -> {Map, Str, "% [integer()]\n"};
        ?FLOAT -> {Map, Str, "% [float()]\n"};
        ?STRING -> {Map, Str, "% [string()]\n"};
        _ -> {maps:put(SubType, 1, Map), Str, "% [#'" ++ SubType ++ "'{}]\n"}
    end;
gen_hrl_field(#field{name = Name, type = Type}, Map) ->
    {maps:put(Type, 1, Map), "\t" ++ Name ++ " = undefined", "% #'" ++ Type ++ "'{}\n"}.