%% -*- coding: utf-8 -*-

-module(xlsx_to_erl).

-include_lib("xmerl/include/xmerl.hrl").

-define(ERL_DIR, "../server_config/").
-define(ETS_MOD_STATE, ets_mod_state).
-define(ETS_SHEET_STATE, ets_sheet_state).
-define(ETS_SHEET_STR_LIST, ets_sheet_str_list).

-define(LOADING, 1).
-define(LOADED, 2).

-define(PROCESS_MAX_ROWS, 1024).

-record(table_field, {col, type, name = ""}).


main(_) ->
    case filelib:ensure_dir(?ERL_DIR) of
        ok ->
            ets:new(?ETS_MOD_STATE, [named_table, public]),
            ets:new(?ETS_SHEET_STATE, [named_table, public]),
            ets:new(?ETS_SHEET_STR_LIST, [named_table, public]),
            {ok, Files} = file:list_dir("./"),
            xlsx_to_erl("./", Files),
            {_, Start, _} = erlang:timestamp(),
            wait_tables(Start);
        _ ->
            io:format("error! dir " ++ ?ERL_DIR ++ " no exist!~n")
    end.

wait_tables(Start) ->
    Fun = fun({_, State}, Flag) -> State =:= ?LOADED andalso Flag end,
    case ets:foldl(Fun, true, ?ETS_MOD_STATE) of
        true ->
            {_, End, _} = erlang:timestamp(),
            io:format("transform all table end, use ~w s~n", [End - Start]);
        false ->
            timer:sleep(100),
            wait_tables(Start)
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
                    set_mod_state(Mod, ?LOADING),
                    spawn(fun() -> spawn_xlsx_to_erl(FileName, Mod) end);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

spawn_xlsx_to_erl(FileName, Mod) ->
    case zip:unzip(FileName, [memory]) of
        {ok, FileBins} ->
            V = proplists:get_value("xl/sharedStrings.xml", FileBins, undefined),
            Share = shared(V),
            xml_to_erl(Mod, FileBins, Share, 1, []);
        _ ->
            set_mod_state(Mod, ?LOADED)
    end.

shared(undefined) -> #{};
shared(Share) ->
    {SST, _Rest} = xmerl_scan:string(binary_to_list(Share)),
    {_, ShareMap} = lists:foldl(
        fun(SI, {N, Map}) ->
            {N + 1, maps:put(N, unicode:characters_to_binary(shared_content(SI)), Map)}
        end, {1, #{}}, SST#xmlElement.content),
    ShareMap.

shared_content([]) -> [];
shared_content(#xmlText{value = Value}) -> Value;
shared_content(#xmlElement{content = Content}) -> shared_content(Content);
shared_content(L) -> [shared_content(T) || T <- L].

xml_to_erl(Mod, FileBins, Share, Index, IdxList) ->
    SheetName = "sheet" ++ to_list(Index),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FileBins, undefined) of
        undefined ->
            wait_sheets(Mod, IdxList);
        SheetBin ->
            set_sheet_state(Mod, Index, ?LOADING),
            spawn(fun() -> xml_to_erl(Mod, Share, Index, SheetBin) end),
            xml_to_erl(Mod, FileBins, Share, Index + 1, [Index | IdxList])
    end.

wait_sheets(Mod, IdxList) ->
    States = [get_sheet_state(Mod, Idx) || Idx <- IdxList],
    LoadedList = [State || State <- States, State =:= ?LOADED],
    case length(LoadedList) >= length(IdxList) of
        true ->
            {StrList0, StrList1} = lists:foldr(
                fun(Index, {Acc00, Acc01}) ->
                    Idx2List = get_sheet_str_list(Mod, Index),
                    del_sheet_str_list(Mod, Index),
                    lists:foldl(
                        fun(Idx2, {Acc10, Acc11}) ->
                            {List0, List1} = get_sheet_str_list(Mod, {Index, Idx2}),
                            del_sheet_str_list(Mod, {Index, Idx2}),
                            {[string:join(List0, "\r\n")|Acc10], [string:join(List1, ",") | Acc11]}
                        end, {Acc00, Acc01}, Idx2List)
                end, {[], []}, IdxList),
            ModName = "tb_" ++ Mod,
            StrHeader = gen_erl_header(ModName),
            StrBody = string:join(lists:reverse(StrList0), "\r\n"),
            StrTail = "\r\nget(_) ->\r\n\tundefined.\r\n\r\nget_list() ->\r\n\t[" ++ string:join(lists:reverse(StrList1), ",") ++ "].",
            ErlFileName = ?ERL_DIR ++ ModName ++ ".erl",
            file:write_file(ErlFileName, unicode:characters_to_binary(StrHeader ++ StrBody ++ StrTail, utf8)),
            lists:foreach(fun(Index) -> del_sheet_state(Mod, Index) end, IdxList),
            del_mod_state(Mod),
            io:format("transform table ~ts success~n", [Mod]);
        false ->
            timer:sleep(100),
            wait_sheets(Mod, IdxList)
    end.

gen_erl_header(RecordName) ->
    "%% -*- coding: utf-8 -*-
-module(" ++ RecordName ++ ").\r\n
-export([get/1,get_list/0]).\r\n\r\n".

xml_to_erl(Mod, Share, Index, SheetBin) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(SheetBin)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    AllRows = sheet_rows(SheetData#xmlElement.content, Share),
    case AllRows of
        [_,FieldRow,TypeRow,Row1,Row2|Rows] when length(Rows) > 0 ->
            Fields1 = to_fields(FieldRow, Row1, TypeRow),
            case lists:keysort(#table_field.col, Fields1) of
                [KeyField1|Types1] when length(Types1) > 0 ->
                    #table_field{col = KeyCol1, type = KeyType1} = KeyField1,
                    xml_to_erl(Mod, Index, KeyCol1, KeyType1, Types1, 1, [], [Row2|Rows]);
                _ ->
                    Fields2 = to_fields(FieldRow, Row2, TypeRow),
                    case lists:keysort(#table_field.col, Fields2) of
                        [KeyField2|Types2] when length(Types2) > 0 ->
                            #table_field{col = KeyCol2, type = KeyType2} = KeyField2,
                            xml_to_erl(Mod, Index, KeyCol2, KeyType2, Types2, 1, [], Rows);
                        _ ->
                            set_sheet_state(Mod, Index, ?LOADED)
                    end
            end;
        _ ->
            set_sheet_state(Mod, Index, ?LOADED)
    end.

to_fields(FieldMap, LoadMap, TypeMap) ->
    maps:fold(
        fun(Col, Field, Acc) ->
            FieldName = to_list(Field),
            Load = maps:get(Col, LoadMap, <<"NULL">>),
            LoadName = to_list(Load),
            Type = to_list(maps:get(Col, TypeMap, <<"NULL">>)),
            case string:str(LoadName, FieldName) > 0 of
                true -> to_fields(Col, list_to_atom(Type), FieldName, Acc);
                false -> Acc
            end
        end, [], FieldMap).

to_fields(Col, int, Name, Fields) ->
    [#table_field{col = Col, type = int, name = Name}|Fields];
to_fields(Col, long, Name, Fields) ->
    [#table_field{col = Col, type = int, name = Name}|Fields];
to_fields(Col, float, Name, Fields) ->
    [#table_field{col = Col, type = float, name = Name}|Fields];
to_fields(Col, string, Name, Fields) ->
    [#table_field{col = Col, type = string, name = Name}|Fields];
to_fields(Col, json, Name, Fields) ->
    [#table_field{col = Col, type = list, name = Name}|Fields];
to_fields(_Col, _Type, _Name, Fields) ->
    Fields.


xml_to_erl(Mod, Idx1, _KeyCol, _KeyType, _Types, _Idx2, Idx2List, []) ->
    wait_sheet_index(Mod, Idx1, Idx2List);
xml_to_erl(Mod, Idx1, KeyCol, KeyType, Types, Idx2, Idx2List, Rows) ->
    {LoadRows, NewRows} = case length(Rows) > ?PROCESS_MAX_ROWS of
                              true -> lists:split(?PROCESS_MAX_ROWS, Rows);
                              false -> {Rows, []}
                          end,
    set_sheet_state(Mod, {Idx1, Idx2}, ?LOADING),
    spawn(fun() -> xml_to_erl(Mod, Idx1, KeyCol, KeyType, Types, Idx2, LoadRows) end),
    xml_to_erl(Mod, Idx1, KeyCol, KeyType, Types, Idx2 + 1, [Idx2 | Idx2List], NewRows).

wait_sheet_index(Mod, Idx1, Idx2List) ->
    States = [get_sheet_state(Mod, {Idx1, Idx2}) || Idx2 <- Idx2List],
    LoadedList = [State || State <- States, State =:= ?LOADED],
    case length(LoadedList) >= length(Idx2List) of
        true ->
            set_sheet_str_list(Mod, Idx1, Idx2List),
            io:format("transform table ~ts sheet ~w success~n", [Mod, Idx1]),
            set_sheet_state(Mod, Idx1, ?LOADED);
        false ->
            timer:sleep(100),
            wait_sheet_index(Mod, Idx1, Idx2List)
    end.

xml_to_erl(Mod, Idx1, KeyCol, KeyType, Types, Idx2, Rows) ->
    StrList = lists:foldr(
        fun(ColMap, {Acc0, Acc1}) ->
            case maps:get(KeyCol, ColMap, null) of
                null ->
                    {Acc0, Acc1};
                Key ->
                    KeyStr = to_key_str(KeyType, Key),
                    NewKeyStr = to_number_str(KeyType, KeyStr),
                    HeaderStr = "get(" ++ NewKeyStr ++ ") -> #{\r\n\t",
                    BodyStrList = gen_record_value_str(Types, ColMap),
                    TailStr = "\r\n};",
                    BodyStr = string:join(BodyStrList, ",\r\n\t"),
                    Str = HeaderStr ++ BodyStr ++ TailStr,
                    {[Str | Acc0], [NewKeyStr | Acc1]}
            end
        end, {[], []}, Rows),
    set_sheet_str_list(Mod, {Idx1, Idx2}, StrList),
    io:format("transform table ~ts sheet ~w index ~w success~n", [Mod, Idx1, Idx2]),
    set_sheet_state(Mod, {Idx1, Idx2}, ?LOADED).

gen_record_value_str(Types, RowDataMap) ->
    lists:foldr(
        fun(#table_field{col = Col, name = Name, type = Type}, Acc) ->
            Value = maps:get(Col, RowDataMap, def(Type)),
            Str = to_str(Type, Value),
            NewStr = to_number_str(Type, Str),
            [Name ++ " => " ++ NewStr | Acc]
        end, [], Types).

def(int) -> "0";
def(float) -> "0.0";
def(string) -> "\"\"";
def(list) -> "[]".

sheet_rows(Rows, Share) ->
    [sheet_row(Share, Row) || Row <- Rows].

sheet_row(Share, Row) ->
    lists:foldl(
        fun(C, Acc) ->
            {Col, V} = sheet_c(Share, C),
            maps:put(Col, V, Acc)
        end, #{}, Row#xmlElement.content).

sheet_c(Share, C) ->
    T = lists:keyfind(t, #xmlAttribute.name, C#xmlElement.attributes),
    V = lists:keyfind(v, #xmlElement.name,   C#xmlElement.content),
    #xmlAttribute{value = Pos} = lists:keyfind(r, #xmlAttribute.name, C#xmlElement.attributes),
    sheet_v(Share, col(Pos), T, V).

sheet_v(Share, Col, #xmlAttribute{value = "s"}, #xmlElement{content = [R]}) ->
    N = list_to_integer(R#xmlText.value),
    {Col, maps:get(N + 1, Share)};
sheet_v(_Share, Col, _, #xmlElement{content = [R]}) ->
    {Col, R#xmlText.value};
sheet_v(_Share, Col, _V1, _V2) ->
    {Col, null}.

col(Pos) ->
    list_to_atom([V || V <- Pos, V >= $A andalso V =< $Z]).


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
            case to_list(Value) of
                [$"|Tail] ->
                    [_|Str] = lists:reverse(Tail),
                    "\"" ++ io_lib:format("~ts", [list_to_binary(lists:reverse(Str))]) ++ "\"";
                _ ->
                    "\"" ++ io_lib:format("~ts", [Value]) ++ "\""
            end;
        false ->
            case to_list(Value) of
                [$"|_] = Str -> Str;
                ValStr -> "\"" ++ ValStr ++ "\""
            end
    end;
to_str(list, Value) ->
    io_lib:format("~ts", [Value]);
to_str(_, Value) ->
    to_list(Value).

to_number_str(int, Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            case lists:member($., Str) of
                true ->
                    integer_to_list(round(list_to_float(Str)));
                false ->
                    case [S || S <- Str, S >= $0 andalso S =< $9] of
                        [] -> "0";
                        NewStr -> NewStr
                    end
            end
    end;
to_number_str(float, Str) ->
    case string:tokens(Str, "E") of
        [V1, V2] ->
            to_num_str(V1, V2);
        _ ->
            case lists:member($., Str) of
                true -> Str;
                false ->
                    case [S || S <- Str, S >= $0 andalso S =< $9] of
                        [] -> "0.0";
                        NewStr -> NewStr ++ ".0"
                    end
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
    set_cache(?ETS_MOD_STATE, list_to_atom(Mod), State).

del_mod_state(Mod) ->
    del_cache(?ETS_MOD_STATE, list_to_atom(Mod)).

get_sheet_state(Mod, Index) ->
    get_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}, ?LOADING).

set_sheet_state(Mod, Index, State) ->
    set_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}, State).

del_sheet_state(Mod, Index) ->
    del_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}).

get_sheet_str_list(Mod, Index) ->
    get_cache(?ETS_SHEET_STR_LIST, {list_to_atom(Mod), Index}, []).

set_sheet_str_list(Mod, Index, KeyList) ->
    set_cache(?ETS_SHEET_STR_LIST, {list_to_atom(Mod), Index}, KeyList).

del_sheet_str_list(Mod, Index) ->
    del_cache(?ETS_SHEET_STR_LIST, {list_to_atom(Mod), Index}).

%%%%%%%%%%%
get_cache(Ets, Key, Def) ->
    case ets:lookup(Ets, Key) of
        [{Key, Value}] -> Value;
        _Other -> Def
    end.

set_cache(Ets, Key, Value) ->
    ets:insert(Ets, {Key, Value}).

del_cache(Ets, Key) ->
    ets:delete(Ets, Key).