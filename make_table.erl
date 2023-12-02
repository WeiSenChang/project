%% -*- coding: utf-8 -*-

-module(make_table).

-include("./include/common.hrl").
-include("./include/game_table.hrl").

-define(MOD, "db_table").
-define(ERL_DIR, "./src/data/").
-define(HRL_DIR, "./include/").

-record(str, {v1 = "", v2 = "", v3 = "", v4 = "", v5 = "", v6 = "", v7 = ""}).

main(_) ->
    create_hrl(),
    create_erl(),
    ok.

create_erl() ->
    Str = gen_erl_header() ++ gen_erl_body() ++ gen_erl_tail(),
    file:write_file(?ERL_DIR ++ ?MOD ++ ".erl", unicode:characters_to_binary(Str, utf8)),
    ok.

gen_erl_header() ->
    "%% -*- coding: utf-8 -*-\r\n
-module(" ++ ?MOD ++ ").\r\n
-include(\"" ++ ?MOD ++ ".hrl\").\r\n
-include(\"db.hrl\").\r\n
-export([
    role_tabs/0,
    sys_tabs/0,
    get_table/1,
    field_map_to_record/2,
    get_fields/1,
    get_field_map/1
]).\r\n\r\n".

gen_erl_body() ->
    {RoleTabs, SysTabs} = split_tabs(),
    Tabs = RoleTabs ++ SysTabs,
    Str = gen_erl_body(#str{}, #{}, Tabs),
    Str0 = gen_tabs_str(RoleTabs, "role"),
    Str1 = gen_tabs_str(SysTabs, "sys"),
    Str2 = Str#str.v1 ++ "get_table(_)->\r\n\t#table{}.\r\n\r\n",
    Str3 = Str#str.v2 ++ Str#str.v3 ++ "get_fields(_) ->\r\n\t[].\r\n\r\n",
    Str4 = Str#str.v4 ++ Str#str.v5 ++ "get_field_map(_) ->\r\n\t#{}.\r\n\r\n",
    Str5 = Str#str.v6 ++ Str#str.v7 ++ "field_map_to_record(_, _FieldMap) ->\r\n\tundefined.\r\n\r\n",
    Str0 ++ Str1 ++ Str2 ++ Str3 ++ Str4 ++ Str5.

gen_erl_body(Str, _, []) ->
    Str;
gen_erl_body(Str, RecordMap, [Tab | Tail]) ->
    #table{key = Key, secs = Secs, fields = Fields} = table(Tab),
    SecsStr = erlang:integer_to_list(Secs),
    V1 = "get_table(" ++ Tab ++ ") ->
    #table{key = " ++ Key ++ ", secs = " ++ SecsStr ++ "};\r\n",
    {NewRecordMap, AddStr} = gen_fields_erl(Tab, RecordMap, #str{v1 = V1}, Fields),
    NewStr = add_str(Str, AddStr),
    gen_erl_body(NewStr, NewRecordMap, Tail).


gen_erl_tail() ->
    "get_field_value(Key, FieldMap) ->
    #field{value = Value} = maps:get(Key, FieldMap),
    Value.\r\n".

split_tabs() ->
    split_tabs([], [], tables()).
split_tabs(RoleTabs, SysTabs, []) ->
    {lists:reverse(RoleTabs), lists:reverse(SysTabs)};
split_tabs(RoleTabs, SysTabs, [Tab | Tail]) ->
    case table(Tab) of
        #table{type = ?ROLE_TAB} ->
            split_tabs([Tab | RoleTabs], SysTabs, Tail);
        #table{type = ?SYS_TAB} ->
            split_tabs(RoleTabs, [Tab | SysTabs], Tail);
        _ ->
            split_tabs(RoleTabs, SysTabs, Tail)
    end.

gen_tabs_str(Tabs, Type) ->
    Body = gen_tabs_str("", 1, length(Tabs), Tabs),
    Type ++ "_tabs() ->\r\n\t[" ++ Body ++ "].\r\n\r\n".
gen_tabs_str(Body, _Count, _Len, []) ->
    Body;
gen_tabs_str(Body, Count, Len, [Tab | Tail]) ->
    case Count >= Len of
        true ->
            gen_tabs_str(Body ++ Tab, Count + 1, Len, Tail);
        false ->
            gen_tabs_str(Body ++ Tab ++ ", ", Count + 1, Len, Tail)
    end.

gen_fields_erl(_Tab, RecordMap, Str, []) ->
    {RecordMap, Str};
gen_fields_erl(Name, RecordMap, Str, Fields) ->
    V2 = "get_fields(Record) when is_record(Record, " ++ Name ++ ") ->\r\n",
    V4 = "get_field_map(" ++ Name ++ ") ->\r\n\t#{\r\n",
    V6 = "field_map_to_record(" ++ Name ++ ", FieldMap) ->\r\n\t#" ++ Name ++ "{\r\n",
    {AddRecordMap, OtherStr} = gen_fields_erl(#{}, #str{}, 1, length(Fields), Fields),
    NewV2 = V2 ++ "\t#" ++ Name ++ "{" ++ OtherStr#str.v1 ++ "\t[\r\n" ++ OtherStr#str.v2,
    NewV4 = V4 ++ OtherStr#str.v3,
    NewV6 = V6 ++ OtherStr#str.v4,
    NewStr = Str#str{v2 = Str#str.v2 ++ NewV2, v4 = Str#str.v4 ++ NewV4, v6 = Str#str.v6 ++ NewV6},
    {NewRecordMap, AddStr} = gen_fields_erl(RecordMap, #str{}, maps:to_list(AddRecordMap)),
    {NewRecordMap, add_str(NewStr, AddStr)}.

gen_fields_erl(RecordMap, Str, []) ->
    {RecordMap, Str};
gen_fields_erl(RecordMap, Str, [{Record, _} | Tail]) ->
    case maps:is_key(Record, RecordMap) of
        false ->
            NewRecordMap0 = maps:put(Record, 1, RecordMap),
            #table{fields = Fields} = record(Record),
            {NewRecordMap1, AddStr} = gen_fields_erl(Record, NewRecordMap0, #str{}, Fields),
            NewStr0 = add_str(Str, AddStr),
            NewStr1 = #str{
                v3 = NewStr0#str.v2 ++ NewStr0#str.v3,
                v5 = NewStr0#str.v4 ++ NewStr0#str.v5,
                v7 = NewStr0#str.v6 ++ NewStr0#str.v7
            },
            gen_fields_erl(NewRecordMap1, NewStr1, Tail);
        _ ->
            gen_fields_erl(RecordMap, Str, Tail)
    end.

add_str(Str0, Str1) ->
    #str{
        v1 = Str0#str.v1 ++ Str1#str.v1,
        v2 = Str0#str.v2 ++ Str1#str.v2,
        v3 = Str0#str.v3 ++ Str1#str.v3,
        v4 = Str0#str.v4 ++ Str1#str.v4,
        v5 = Str0#str.v5 ++ Str1#str.v5,
        v6 = Str0#str.v6 ++ Str1#str.v6,
        v7 = Str0#str.v7 ++ Str1#str.v7
    }.

gen_fields_erl(AddRecordMap, Str, _Count, _Len, []) ->
    {AddRecordMap, Str};
gen_fields_erl(AddRecordMap, Str, Count, Len, [Field | Tail]) ->
    {NewAddRecordMap, AddStr} = gen_field_erl(Field, Count, Len, AddRecordMap),
    gen_fields_erl(NewAddRecordMap, add_str(Str, AddStr), Count + 1, Len, Tail).

gen_field_erl(Field, Count, Len, Map) ->
    #field{name = Name, type = Type, sub_type = SubType} = Field,
    IndexStr = erlang:integer_to_list(Count),
    {NewMap, SubTypeStr} = update_record_map(Type, SubType, Map),
    TypeStr = erlang:atom_to_list(Type),
    V1 = Name ++ " = F" ++ IndexStr,
    V2 = "\t\t#field{name = " ++ Name ++ ", type = " ++ TypeStr ++ ", sub_type = " ++ SubTypeStr ++ ", value = F"++ IndexStr ++ "}",
    V3 = "\t\t" ++ Name ++ " => #field{name = " ++ Name ++ ", type = " ++ TypeStr ++ ", sub_type = " ++ SubTypeStr ++ "}",
    V4 = "\t\t" ++ Name ++ " = get_field_value(" ++ Name ++ ", FieldMap)",
    {V1Tail, V2Tail, V3Tail, V4Tail} =
        case Count < Len of
            true ->
                {", ", ",\r\n", ",\r\n", ",\r\n"};
            false ->
                {"} = Record,\r\n", "\r\n\t];\r\n", "\r\n\t};\r\n", "\r\n\t};\r\n"}
        end,
    {NewMap, #str{v1 = V1 ++ V1Tail, v2 = V2 ++ V2Tail, v3 = V3 ++ V3Tail, v4 = V4 ++ V4Tail}}.

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
    Body = gen_hrl_body(),
    Str = Header ++ Body#str.v1 ++ Body#str.v2 ++ Body#str.v3 ++ "\r\n-endif.\r\n",
    file:write_file(?HRL_DIR ++ ?MOD ++ ".hrl", unicode:characters_to_binary(Str, utf8)).

gen_hrl_header() ->
    "%% -*- coding: utf-8 -*-\r\n
-ifndef('" ++ ?MOD ++ "_HRL').
-define('" ++ ?MOD ++ "_HRL', true).\r\n\r\n".

gen_hrl_body() ->
    gen_hrl_body(#{}, #str{}, tables()).
gen_hrl_body(_, Str, []) ->
    Str;
gen_hrl_body(RecordMap, Str, [Tab | Tail]) ->
    #table{fields = Fields} = table(Tab),
    V1 = "-define(" ++ string:to_upper(Tab) ++ ", " ++ Tab ++ ").\r\n",
    {NewRecordMap, AddStr} = gen_hrl_record(Tab, RecordMap, #str{v1 = V1}, Fields),
    gen_hrl_body(NewRecordMap, add_str(Str, AddStr), Tail).

%%
gen_hrl_record(_Name, RecordMap, Str, []) ->
    {RecordMap, Str};
gen_hrl_record(Name, RecordMap, Str, Fields) ->
    V2 = "\r\n-record(" ++ Name ++ ", {\r\n",
    {AddRecordMap, OtherStr} = gen_hrl_record(#{}, "", 1, length(Fields), Fields),
    NewStr = Str#str{v2 = Str#str.v2 ++ V2 ++ OtherStr ++ "}).\r\n"},
    {NewRecordMap, AddStr} = gen_hrl_record(RecordMap, #str{}, maps:to_list(AddRecordMap)),
    {NewRecordMap, add_str(NewStr, AddStr)}.

gen_hrl_record(RecordMap, Str, []) ->
    {RecordMap, Str};
gen_hrl_record(RecordMap, Str, [{Record, _} | Tail]) ->
    case maps:is_key(Record, RecordMap) of
        false ->
            NewRecordMap0 = maps:put(Record, 1, RecordMap),
            #table{fields = Fields} = record(Record),
            {NewRecordMap1, AddStr} = gen_hrl_record(Record, NewRecordMap0, #str{}, Fields),
            NewStr = Str#str{v3 = Str#str.v3 ++ AddStr#str.v2 ++ AddStr#str.v3},
            gen_hrl_record(NewRecordMap1, NewStr, Tail);
        true ->
            gen_hrl_record(RecordMap, Str, Tail)
    end.

gen_hrl_record(RecordMap, Str, _, _, []) ->
    {RecordMap, Str};
gen_hrl_record(RecordMap, Str, Count, Len, [Field | Tail]) ->
    {NewRecordMap, Str0, Str1} = gen_hrl_field(Field, RecordMap),
    NewStr =
        case Count < Len of
            true -> Str ++ Str0 ++ ", " ++ Str1;
            false -> Str ++ Str0 ++ " " ++ Str1
        end,
    gen_hrl_record(NewRecordMap, NewStr, Count + 1, Len, Tail).

%%
gen_hrl_field(#field{name = Name, type = ?INT}, Map) ->
    {Map, "\t" ++ Name ++ " = 0", "\r\n"};
gen_hrl_field(#field{name = Name, type = ?FLOAT}, Map) ->
    {Map, "\t" ++ Name ++ " = 0.0", "\r\n"};
gen_hrl_field(#field{name = Name, type = ?STRING}, Map) ->
    {Map, "\t" ++ Name ++ " = \"\"", "\r\n"};
gen_hrl_field(#field{name = Name, type = ?LIST, sub_type = SubType}, Map) ->
    Str = "\t" ++ Name ++ " = []",
    case SubType of
        ?INT -> {Map, Str, "% [integer()]\r\n"};
        ?FLOAT -> {Map, Str, "% [float()]\r\n"};
        ?STRING -> {Map, Str, "% [string()]\r\n"};
        _ -> {maps:put(SubType, 1, Map), Str, "% [#" ++ SubType ++ "{}]\r\n"}
    end;
gen_hrl_field(#field{name = Name, type = Type}, Map) ->
    {maps:put(Type, 1, Map), "\t" ++ Name ++ " = undefined", "% #" ++ Type ++ "{}\r\n"}.