%% -*- coding: utf-8 -*-

-module(xlsx_to_erl).

-include_lib("xmerl/include/xmerl.hrl").

-define(ERL_DIR, "../server_config/").
-define(ETS_MOD, ets_mod).

-define(LOADING, 1).
-define(LOADED, 2).


-record(table, {key_type, type_map = #{}, rows = []}).
-record(table_field, {col, type, name = ""}).


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
            case xml_to_erl(FileBin) of
                {Share, Tables} when length(Tables) > 0 ->
                    sheets_to_erl(Share, Mod, Tables),
                    io:format("transform ~ts success~n", [Mod]);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

xml_to_erl(FileBin) ->
    V = proplists:get_value("xl/sharedStrings.xml", FileBin, undefined),
    Share = clean_share(V),
    Amount = 1,
    Tables = xml_to_erl(FileBin, Share, Amount, []),
    {Share, lists:reverse(Tables)}.

xml_to_erl(FileBin, Share, Amount, Tables) ->
    SheetName = "sheet" ++ to_list(Amount),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FileBin, undefined) of
        undefined ->
            Tables;
        SheetXML ->
            NewTables = clean_sheet(Tables, Share, SheetXML),
            xml_to_erl(FileBin, Share, Amount + 1, NewTables)
    end.

clean_share(undefined) -> #{};
clean_share(Share) ->
    {SST, _Rest} = xmerl_scan:string(binary_to_list(Share)),
    {_, ShareMap} = lists:foldl(
        fun(SI, {N, Map}) ->
            {N + 1, maps:put(N, unicode:characters_to_binary(clean_share_content(SI)), Map)}
        end, {1, #{}}, SST#xmlElement.content),
    ShareMap.

clean_share_content([]) -> [];
clean_share_content(#xmlText{value = Value}) -> Value;
clean_share_content(#xmlElement{content = Content}) -> clean_share_content(Content);
clean_share_content(L) -> [clean_share_content(T) || T <- L].

clean_sheet(Tables, _Share, undefined) ->
    Tables;
clean_sheet(Tables, Share, Sheet) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(Sheet)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    case SheetData#xmlElement.content of
        [_,FieldRow,TypeRow,_,_|Rows] when length(Rows) > 0 ->
            case get_field_info(FieldRow, TypeRow, Share) of
                {KeyType, TypeMap} when map_size(TypeMap) > 0 ->
                    [#table{key_type = KeyType, type_map = TypeMap, rows = Rows} | Tables];
                _ ->
                    Tables
            end;
        _ ->
            Tables
    end.

get_field_info(FieldRow, TypeRow, Share) ->
    FieldMap = clean_sheet_row(Share, FieldRow),
    TypeMap = clean_sheet_row(Share, TypeRow),
    NewTypeMap = to_type_map(TypeMap),
    NewFieldMap = to_field_map(FieldMap, NewTypeMap),
    case lists:sort(maps:keys(NewFieldMap)) of
        [KeyCol|FieldCols] when length(FieldCols) > 0 ->
            {maps:get(KeyCol, NewFieldMap), maps:remove(KeyCol, NewFieldMap)};
        _ ->
            {#table_field{}, #{}}
    end.

to_type_map(TypeMap) ->
    maps:fold(
        fun(Col, Type, Acc) ->
            case Type of
                <<"int">> ->
                    maps:put(Col, int, Acc);
                <<"long">> ->
                    maps:put(Col, int, Acc);
                <<"float">> ->
                    maps:put(Col, float, Acc);
                <<"string">> ->
                    maps:put(Col, string, Acc);
                <<"json">> ->
                    maps:put(Col, list, Acc);
                _ ->
                    Acc
            end
        end, #{}, TypeMap).

to_field_map(FieldMap, TypeMap) ->
    maps:fold(
        fun(Col, Type, Acc) ->
            Field = maps:get(Col, FieldMap),
            Name = to_list(Field),
            TableField = #table_field{col = Col, type = Type, name = Name},
            maps:put(Col, TableField, Acc)
        end, #{}, TypeMap).

clean_sheet_row(Share, Row) ->
    lists:foldl(
        fun(C, Acc) ->
            {Col, V} = clean_sheet_c(Share, C),
            maps:put(Col, V, Acc)
        end, #{}, Row#xmlElement.content).

clean_sheet_c(Share, C) ->
    T = lists:keyfind(t, #xmlAttribute.name, C#xmlElement.attributes),
    V = lists:keyfind(v, #xmlElement.name,   C#xmlElement.content),
    #xmlAttribute{value = Pos} = lists:keyfind(r, #xmlAttribute.name, C#xmlElement.attributes),
    clean_sheet_v(Share, col(Pos), T, V).

clean_sheet_v(Share, Col, #xmlAttribute{value = "s"}, #xmlElement{content = [R]}) ->
    N = list_to_integer(R#xmlText.value),
    {Col, maps:get(N + 1, Share)};
clean_sheet_v(_Share, Col, _, #xmlElement{content = [R]}) ->
    {Col, R#xmlText.value};
clean_sheet_v(_Share, Col, _V1, _V2) ->
    {Col, null}.

col(Pos) ->
    list_to_atom([V || V <- Pos, V >= $A andalso V =< $Z]).

sheets_to_erl(Share, Name, Tables) ->
    Mod = "cfg_" ++ Name,
    FileName = ?ERL_DIR ++ Mod ++ ".erl",
    {ok, Fd} = file:open(FileName, [write]),
    ErlHeader = gen_erl_header(Mod),
    file:write(Fd, unicode:characters_to_binary(ErlHeader, utf8)),
    KeyStrList = sheets_to_erl(Fd, Share, [], Tables),
    Str = "get(_) ->\r\n\tundefined.\r\n
    get_list() ->\r\n\t[" ++ string:join(KeyStrList, ",") ++ "].",
    file:write(Fd, unicode:characters_to_binary(Str, utf8)),
    file:close(Fd),
    ok.

gen_erl_header(RecordName) ->
    "%% -*- coding: utf-8 -*-
-module(" ++ RecordName ++ ").\r\n
-export([get/1,get_list/0]).\r\n\r\n".

sheets_to_erl(_Fd, _Share, KeyStrList, []) ->
    lists:reverse(KeyStrList);
sheets_to_erl(Fd, Share, KeyStrList, [Table | Tail]) ->
    #table{key_type = #table_field{col = KeyCol, type = KeyType}, type_map = TypeMap, rows = Rows} = Table,
    NewKeyStrList = sheets_to_erl(Fd, Share, KeyCol, KeyType, KeyStrList, TypeMap, Rows),
    sheets_to_erl(Fd, Share, NewKeyStrList, Tail).

sheets_to_erl(_Fd, _Share, _KeyCol, _Type, KeyStrList, _, []) ->
    KeyStrList;
sheets_to_erl(Fd, Share, KeyCol, KeyType, KeyStrList, TypeMap, [Row | Tail]) ->
    RowDataMap = clean_sheet_row(Share, Row),
    case maps:get(KeyCol, RowDataMap, null) of
        null ->
            sheets_to_erl(Fd, Share, KeyCol, KeyType, KeyStrList, TypeMap, Tail);
        Key ->
            KeyStr = to_key_str(KeyType, Key),
            HeaderStr = "get(" ++ KeyStr ++ ") -> #{\r\n\t",
            BodyStrList = gen_record_value_str(TypeMap, RowDataMap),
            TailStr = "\r\n};\r\n",
            BodyStr = string:join(BodyStrList, ",\r\n\t"),
            Str = HeaderStr ++ BodyStr ++ TailStr,
            file:write(Fd, unicode:characters_to_binary(Str, utf8)),
            sheets_to_erl(Fd, Share, KeyCol, KeyType, [KeyStr | KeyStrList], TypeMap, Tail)
    end.

gen_record_value_str(TypeMap, RowDataMap) ->
    maps:fold(
        fun(_, #table_field{col = Col, name = Name, type = Type}, Acc) ->
            case maps:get(Col, RowDataMap, null) of
                null ->
                    Acc;
                Value ->
                    Str = to_str(Type, Value),
                    NewStr = to_number_str(Type, Str),
                    [Name ++ " => " ++ NewStr | Acc]
            end
        end, [], TypeMap).


%%%%%%%%%%%%
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


to_key_str(string, Value) when is_binary(Value) ->
    io_lib:format("~ts", [Value]);
to_key_str(_, Value) ->
    to_list(Value).

to_str(string, Value) ->
    case is_binary(Value) of
        true ->
            "<<\"" ++ io_lib:format("~ts", [Value]) ++ "\"/utf8>>";
        false ->
            "<<\"" ++ to_list(Value) ++ "\"/utf8>>"
    end;
to_str(list, Value) ->
    io_lib:format("~ts", [Value]);
to_str(_, Value) ->
    to_list(Value).

to_number_str(int, Str) when length(Str) =< 0 ->
    "0";
to_number_str(int, Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            case lists:member($., Str) of
                true ->
                    integer_to_list(round(list_to_float(Str)));
                false ->
                    Str
            end
    end;
to_number_str(float, Str) when length(Str) =< 0 ->
    "0.0";
to_number_str(float, Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            case lists:member($., Str) of
                false ->
                    Str ++ ".0";
                true ->
                    Str
            end
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


%%%%%%%%
set_mod_state(Mod, State) ->
    ets:insert(?ETS_MOD, {Mod, State}).