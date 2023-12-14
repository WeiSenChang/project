%% -*- coding: utf-8 -*-

-module(xlsx_to_erl).

-include_lib("xmerl/include/xmerl.hrl").

-define(ERL_DIR, "../server_config/").
-define(ETS_MOD, ets_mod).

-define(LOADING, 1).
-define(LOADED, 2).

main(_) ->
    case filelib:ensure_dir(?ERL_DIR) of
        ok ->
            ets:new(?ETS_MOD, [named_table, public]),
            {ok, Files} = file:list_dir("./"),
            xlsx_to_erl("./", Files),
            timer:sleep(1000),
            wait(1);
        _ ->
            io:format("error! dir " ++ ?ERL_DIR ++ " no exist!~n")
    end.

wait(N) ->
    Fun = fun({_, State}, Flag) -> State =:= ?LOADED andalso Flag end,
    case ets:foldl(Fun, true, ?ETS_MOD) of
        true ->
            io:format("transform all table end~n");
        false ->
            io:format("waiting transform ~w s~n", [N]),
            timer:sleep(1000),
            wait(N + 1)
    end.


xlsx_to_erl(_Dir, []) ->
    ok;
xlsx_to_erl(Dir, [File | Tail]) ->
    do_xlsx_to_erl(Dir, File),
    xlsx_to_erl(Dir, Tail).

do_xlsx_to_erl(Dir, File) ->
    FileName = Dir ++ File,
    case filelib:is_dir(FileName) of
        true ->
            NewDir = FileName ++ "/",
            {ok, Files} = file:list_dir(NewDir),
            xlsx_to_erl(NewDir, Files);
        false ->
            do_xlsx_to_erl_i(FileName, File)
    end.

do_xlsx_to_erl_i(FileName, File) ->
    case string:tokens(File, ".") of
        [Name, "xlsx"] ->
            case string:tokens(Name, "-") of
                [Mod, _] ->
                    do_xlsx_to_erl_ii(FileName, Mod);
                [Mod] ->
                    do_xlsx_to_erl_ii(FileName, Mod);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

do_xlsx_to_erl_ii(FileName, Mod) ->
    spawn(fun() -> spawn_xlsx_to_erl(FileName, Mod) end).

spawn_xlsx_to_erl(FileName, Mod) ->
    set_mod_state(Mod, ?LOADING),
    try
        do_spawn_xlsx_to_erl(FileName, Mod)
    catch
        _:_Error ->
            io:format("transform ~ts error: ~w~n", [Mod, _Error])
    end,
    set_mod_state(Mod, ?LOADED).

do_spawn_xlsx_to_erl(FileName, Mod) ->
    case zip:unzip(FileName, [memory]) of
        {ok, FileBin} ->
            Sheets = xml_to_erl(FileBin),
            sheets_to_erl(Mod, Sheets),
            io:format("transform ~ts success~n", [Mod]);
        _ ->
            ignore
    end.

xml_to_erl(FileBin) ->
    V = proplists:get_value("xl/sharedStrings.xml", FileBin, undefined),
    Share = clean_share(V),
    Amount = 1,
    Tables = xml_to_erl(FileBin, Share, Amount, []),
    lists:reverse(Tables).

xml_to_erl(FileBin, Share, Amount, Tables) ->
    SheetName = "sheet" ++ to_list(Amount),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FileBin, undefined) of
        undefined -> Tables;
        SheetXML ->
            Sheet = clean_sheet(SheetXML),
            Table = {SheetName, pack_table(Share, Sheet)},
            xml_to_erl(FileBin, Share, Amount+1, [Table | Tables])
    end.

clean_share(undefined) -> [];
clean_share(Share) ->
    {SST, _Rest} = xmerl_scan:string(binary_to_list(Share)),
    [unicode:characters_to_binary(lists:flatten(clean_share_content(SI))) || SI <- SST#xmlElement.content].

clean_share_content([]) -> [];
clean_share_content(#xmlText{value = Value}) -> Value;
clean_share_content(#xmlElement{content = Content}) -> clean_share_content(Content);
clean_share_content(L) -> [clean_share_content(T) || T <- L].


clean_sheet(undefined) -> [];
clean_sheet(Sheet) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(Sheet)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    [clean_sheet_row(Row) || Row <- SheetData#xmlElement.content, Row#xmlElement.name == row].

clean_sheet_row(Row) ->
    [clean_sheet_c(C) || C <- Row#xmlElement.content, C#xmlElement.name == c].

clean_sheet_c(C) ->
    T = lists:keyfind(t, #xmlAttribute.name, C#xmlElement.attributes),
    V = lists:keyfind(v, #xmlElement.name,   C#xmlElement.content),
    #xmlAttribute{value = Pos} = lists:keyfind(r, #xmlAttribute.name, C#xmlElement.attributes),
    clean_sheet_v(col(Pos), T, V).

%% 表格没有内容，为 null
clean_sheet_v(Col, #xmlAttribute{value = "s"}, #xmlElement{content = [R]}) ->
    {transform, Col, list_to_integer(R#xmlText.value)};
clean_sheet_v(Col, _, #xmlElement{content = [R]}) ->
    {Col, R#xmlText.value};
clean_sheet_v(Col, _V1, _V2) ->
    {Col, null}.

col(Pos) ->
    [V || V <- Pos, V >= $A andalso V =< $Z].

pack_table(Share, Sheet) ->
    [pack_row(Row, Share) || Row <- Sheet].

pack_row(Row, Share) ->
    [pack_value(V, Share) || V <- Row].

pack_value({transform, Col, V}, Share) -> {Col, lists:nth(V+1, Share)};
pack_value(V, _Share) -> V.


sheets_to_erl(Name, Sheets) ->
    SheetFields = [get_record_fields(Sheet) || {_, Sheet} <- Sheets],
    Mod = "cfg_" ++ Name,
    ErlHeader = gen_erl_header(Mod),
    ErlBody = gen_erl_body(SheetFields, Sheets),
    Str = ErlHeader ++ ErlBody,
    file:write_file(?ERL_DIR ++ Mod ++ ".erl", unicode:characters_to_binary(Str, utf8)),
    ok.

gen_erl_body(SheetFields, Sheets) ->
    {Str0, StrList} = gen_erl_body("", [], SheetFields, Sheets),
    Str1 = "get(_) ->\r\n\tundefined.\r\n\r\n",
    Str2 = "get_list() ->\r\n\t[" ++ string:join(StrList, ",") ++ "].",
    Str0 ++ Str1 ++ Str2.
gen_erl_body(Str0, StrList, [], []) ->
    {Str0, lists:reverse(StrList)};
gen_erl_body(Str0, StrList, [{{KeyCol, _, Type}, Fields} | Tail0], [{_, [_,_,_,_,_|Rows]} | Tail1]) ->
    case length(Fields) > 0 of
        true ->
            {NewStr0, NewStrList} = gen_erl_body(KeyCol, Type, Str0, StrList, Fields, Rows),
            gen_erl_body(NewStr0, NewStrList, Tail0, Tail1);
        false ->
            gen_erl_body(Str0, StrList, Tail0, Tail1)
    end;
gen_erl_body(Str0, StrList, [_ | Tail0], [_ | Tail1]) ->
    gen_erl_body(Str0, StrList, Tail0, Tail1).

gen_erl_body(_KeyCol, _Type, Str0, StrList, _, []) ->
    {Str0, StrList};
gen_erl_body(KeyCol, Type, Str0, StrList, Fields, [Row | Tail]) ->
    case lists:keyfind(KeyCol, 1, Row) of
        {KeyCol, Key} when Key =/= null ->
            KeyStr = to_str(Type, Key),
            NewStrList = [KeyStr | StrList],
            HeaderStr = "get(" ++ KeyStr ++ ") -> #{\r\n\t",
            BodyStrList = gen_record_value_str(Fields, Row),
            TailStr = "\r\n};\r\n",
            BodyStr = string:join(BodyStrList, ",\r\n\t"),
            Str1 = HeaderStr ++ BodyStr ++ TailStr,
            gen_erl_body(KeyCol, Type, Str0 ++ Str1, NewStrList, Fields, Tail);
        _ ->
            gen_erl_body(KeyCol, Type, Str0, StrList, Fields, Tail)
    end.

gen_record_value_str(Fields, Row) ->
    gen_record_value_str([], Row, Fields).
gen_record_value_str(StrList, _Row, []) ->
    lists:reverse(StrList);
gen_record_value_str(StrList, Row, [{Col, Name, Type} | Tail]) ->
    case lists:keyfind(Col, 1, Row) of
        {Col, Value} when Value =/= null ->
            Str = to_str(Type, Value),
            NewStr = to_number_str(Type, Str),
            NewStrList = [Name ++ " => " ++ NewStr | StrList],
            gen_record_value_str(NewStrList, Row, Tail);
        _ ->
            gen_record_value_str(StrList, Row, Tail)
    end.

to_str("string", Value) ->
    case is_binary(Value) of
        true ->
            "\"" ++ io_lib:format("~ts", [Value]) ++ "\"";
        false ->
            "\"" ++ to_list(Value) ++ "\""
    end;
to_str("list", Value) ->
    io_lib:format("~ts", [Value]);
to_str(_, Value) ->
    to_list(Value).

to_number_str("int", Str) when length(Str) =< 0 ->
    "0";
to_number_str("int", Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            Str
    end;
to_number_str("float", Str) when length(Str) =< 0 ->
    "0.0";
to_number_str("float", Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            Str
    end;
to_number_str(_, Str) ->
    Str.

to_num_str(V, [$+|Str]) ->
    Num = list_to_integer(Str),
    Zeros = lists:duplicate(Num, $0),
    case string:tokens(V, ".") of
        [V1, V2] ->
            to_num_str_by_right(V1, V2, Zeros);
        _ ->
            to_num_str_by_right(V, "0", Zeros)
    end;
to_num_str(V, [$-|Str]) ->
    Num = list_to_integer(Str),
    Zeros = lists:duplicate(Num, $0),
    case string:tokens(V, ".") of
        [V1, V2] ->
            to_num_str_by_left(lists:reverse(V1), V2, Zeros);
        _ ->
            to_num_str_by_left("0", V, Zeros)
    end;
to_num_str(V, _) ->
    V.

to_num_str_by_right(V1, [], []) ->
    V1;
to_num_str_by_right(V1, V2, []) ->
    V1 ++ "." ++ V2;
to_num_str_by_right(V1, [C1|T1], [_C2|T2]) ->
    to_num_str_by_right(V1++integer_to_list(C1-48), T1, T2);
to_num_str_by_right(V1, [], [C2|T2]) ->
    to_num_str_by_right(V1++integer_to_list(C2-48), [], T2).

to_num_str_by_left([], V2, []) ->
    [$0, $. | V2];
to_num_str_by_left(V1, V2, []) ->
    "0." ++ V1 ++ V2;
to_num_str_by_left([C1|T1], V2, [_C2|T2]) ->
    to_num_str_by_left(T1, [C1-48|V2], T2);
to_num_str_by_left([], V2, [C2|T2]) ->
    to_num_str_by_left([], [C2-48|V2], T2).


gen_erl_header(RecordName) ->
    "%% -*- coding: utf-8 -*-
-module(" ++ RecordName ++ ").\r\n
-export([get/1,get_list/0]).\r\n\r\n".

get_record_fields([_, Fields, Types | _Tail]) ->
    NewTypes = get_record_types([], Types),
    get_record_fields([], Fields, NewTypes);
get_record_fields(_) ->
    {{"inf", "undefined", "null"}, []}.

get_record_fields([], _Fields, []) ->
    {{"inf", "undefined", "null"}, []};
get_record_fields([KeyField | SheetField], _Fields, []) ->
    {KeyField, SheetField};
get_record_fields(SheetField, Fields, [{Col, Type} | Tail]) ->
    {Col, Filed} = lists:keyfind(Col, 1, Fields),
    NewFiled = to_list(Filed),
    NewSheetField = [{Col, NewFiled, Type} | SheetField],
    get_record_fields(NewSheetField, Fields, Tail).

get_record_types(Types, []) ->
    Types;
get_record_types(Types, [{Col, <<"int">>} | Tail]) ->
    get_record_types([{Col, "int"} | Types], Tail);
get_record_types(Types, [{Col, <<"long">>} | Tail]) ->
    get_record_types([{Col, "int"} | Types], Tail);
get_record_types(Types, [{Col, <<"float">>} | Tail]) ->
    get_record_types([{Col, "float"} | Types], Tail);
get_record_types(Types, [{Col, <<"string">>} | Tail]) ->
    get_record_types([{Col, "string"} | Types], Tail);
get_record_types(Types, [{Col, <<"json">>} | Tail]) ->
    get_record_types([{Col, "list"} | Types], Tail);
get_record_types(Types, [_ | Tail]) ->
    get_record_types(Types, Tail).

to_list(Value) when is_list(Value) ->
    Value;
to_list(Value) when is_binary(Value) ->
    erlang:binary_to_list(Value);
to_list(Value) when is_integer(Value) ->
    erlang:integer_to_list(Value);
to_list(Value) when is_float(Value) ->
    erlang:float_to_list(Value);
to_list(Value) when is_atom(Value) ->
    erlang:atom_to_list(Value);
to_list(Value) ->
    erlang:binary_to_list(erlang:term_to_binary(Value)).

%%%%%%%%
set_mod_state(Mod, State) ->
    ets:insert(?ETS_MOD, {Mod, State}).