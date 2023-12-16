%% -*- coding: utf-8 -*-

-module(xlsx_to_erl).

-include_lib("xmerl/include/xmerl.hrl").

-define(ERL_DIR, "../server_config/").
-define(ETS_MOD_STATE, ets_mod_state).
-define(ETS_SHEET_STATE, ets_sheet_state).
-define(ETS_SHEET_KEY_LIST, ets_sheet_key_list).

-define(LOADING, 1).
-define(LOADED, 2).

-define(PROCESS_MAX_ROWS, 1024).

-record(table_field, {col, type, name = ""}).


main(_) ->
    case filelib:ensure_dir(?ERL_DIR) of
        ok ->
            ets:new(?ETS_MOD_STATE, [named_table, public]),
            ets:new(?ETS_SHEET_STATE, [named_table, public]),
            ets:new(?ETS_SHEET_KEY_LIST, [named_table, public]),
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
            ModName = "tb_" ++ Mod,
            ErlFileName = ?ERL_DIR ++ ModName ++ ".erl",
            {ok, Fd} = file:open(ErlFileName, [write]),
            ErlHeader = gen_erl_header(ModName),
            file:write(Fd, unicode:characters_to_binary(ErlHeader, utf8)),
            V = proplists:get_value("xl/sharedStrings.xml", FileBins, undefined),
            Share = shared(V),
            xml_to_erl(Mod, Fd, FileBins, Share, 1, []);
        _ ->
            set_mod_state(Mod, ?LOADED)
    end.

gen_erl_header(RecordName) ->
    "%% -*- coding: utf-8 -*-
-module(" ++ RecordName ++ ").\r\n
-export([get/1,get_list/0]).\r\n\r\n".

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

xml_to_erl(Mod, Fd, FileBins, Share, Index, IdxList) ->
    SheetName = "sheet" ++ to_list(Index),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FileBins, undefined) of
        undefined ->
            wait_sheets(Mod, Fd, IdxList);
        SheetBin ->
            set_sheet_state(Mod, Index, ?LOADING),
            spawn(fun() -> xml_to_erl(Mod, Fd, Share, Index, SheetBin) end),
            xml_to_erl(Mod, Fd, FileBins, Share, Index + 1, [Index | IdxList])
    end.

wait_sheets(Mod, Fd, IdxList) ->
    States = [get_sheet_state(Mod, Idx) || Idx <- IdxList],
    LoadedList = [State || State <- States, State =:= ?LOADED],
    case length(LoadedList) >= length(IdxList) of
        true ->
            KeyStrList = lists:foldl(
                fun(Index, Acc0) ->
                    Idx2List = get_sheet_key_list(Mod, Index),
                    del_sheet_key_list(Mod, Index),
                    lists:foldr(
                        fun(Idx2, Acc1) ->
                            List = get_sheet_key_list(Mod, {Index, Idx2}),
                            del_sheet_key_list(Mod, {Index, Idx2}),
                            NewList = lists:reverse(List),
                            KeyStr = string:join(NewList, ","),
                            [KeyStr | Acc1]
                        end, Acc0, Idx2List)
                end, [], IdxList),
            ErlTail = "get(_) ->\r\n\tundefined.\r\n\r\nget_list() ->\r\n\t[" ++ string:join(KeyStrList, ",\r\n\t") ++ "].",
            file:write(Fd, unicode:characters_to_binary(ErlTail, utf8)),
            file:close(Fd),
            lists:foreach(fun(Index) -> del_sheet_state(Mod, Index) end, IdxList),
            del_mod_state(Mod),
            io:format("transform table ~ts success~n", [Mod]);
        false ->
            timer:sleep(100),
            wait_sheets(Mod, Fd, IdxList)
    end.

xml_to_erl(Mod, Fd, Share, Index, SheetBin) ->
    {Root, _Rest} = xmerl_scan:string(binary_to_list(SheetBin)),
    SheetData = lists:keyfind(sheetData, #xmlElement.name, Root#xmlElement.content),
    AllRows = sheet_rows(SheetData#xmlElement.content, Share),
    case AllRows of
        [_,FieldRow,TypeRow,_,LoadRow|Rows] when length(Rows) > 0 ->
            case get_field_info(FieldRow, TypeRow, LoadRow) of
                {#table_field{col = KeyCol, type = KeyType}, Types} when length(Types) > 0 ->
                    xml_to_erl(Mod, Fd, Index, KeyCol, KeyType, Types, 1, [], Rows);
                _ ->
                    set_sheet_state(Mod, Index, ?LOADED)
            end;
        _ ->
            set_sheet_state(Mod, Index, ?LOADED)
    end.

xml_to_erl(Mod, _Fd, Idx1, _KeyCol, _KeyType, _Types, _Idx2, Idx2List, []) ->
    wait_sheet_index(Mod, Idx1, Idx2List);
xml_to_erl(Mod, Fd, Idx1, KeyCol, KeyType, Types, Idx2, Idx2List, Rows) ->
    {LoadRows, NewRows} = case length(Rows) > ?PROCESS_MAX_ROWS of
                              true -> lists:split(?PROCESS_MAX_ROWS, Rows);
                              false -> {Rows, []}
                          end,
    spawn(fun() -> xml_to_erl(Mod, Fd, Idx1, KeyCol, KeyType, Types, Idx2, LoadRows) end),
    xml_to_erl(Mod, Fd, Idx1, KeyCol, KeyType, Types, Idx2 + 1, [Idx2 | Idx2List], NewRows).

wait_sheet_index(Mod, Idx1, Idx2List) ->
    States = [get_sheet_state(Mod, {Idx1, Idx2}) || Idx2 <- Idx2List],
    LoadedList = [State || State <- States, State =:= ?LOADED],
    case length(LoadedList) >= length(Idx2List) of
        true ->
            set_sheet_key_list(Mod, Idx1, Idx2List),
            io:format("transform table ~ts sheet ~w success~n", [Mod, Idx1]),
            set_sheet_state(Mod, Idx1, ?LOADED);
        false ->
            timer:sleep(100),
            wait_sheet_index(Mod, Idx1, Idx2List)
    end.

xml_to_erl(Mod, Fd, Idx1, KeyCol, KeyType, Types, Idx2, Rows) ->
    {StrList0, StrList1} = lists:foldl(
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
    SheetIdxStr = string:join(StrList0, "\r\n"),
    file:write(Fd, unicode:characters_to_binary(SheetIdxStr, utf8)),
    set_sheet_key_list(Mod, {Idx1, Idx2}, StrList1),
    io:format("transform table ~ts sheet ~w index ~w success~n", [Mod, Idx1, Idx2]),
    set_sheet_state(Mod, {Idx1, Idx2}, ?LOADED).

get_field_info(FieldMap, TypeMap, LoadMap) ->
    NewTypeMap = to_type_map(TypeMap, LoadMap),
    Fields = to_fields(FieldMap, NewTypeMap),
    case lists:keysort(#table_field.col, Fields) of
        [KeyField|NewFields] when length(NewFields) > 0 ->
            {KeyField, NewFields};
        _ ->
            {#table_field{}, []}
    end.

to_type_map(TypeMap, LoadMap) ->
    maps:fold(
        fun(Col, Type, Acc) ->
            case maps:get(Col, LoadMap, null) of
                null -> Acc;
                Field ->
                    case to_list(Field) of
                        [_|_] ->
                            to_type_map(Col, Type, Acc);
                        _ ->
                            Acc
                    end
            end
        end, #{}, TypeMap).

to_type_map(Col, Type, TypeMap) ->
    case Type of
        <<"int">> ->
            maps:put(Col, int, TypeMap);
        <<"long">> ->
            maps:put(Col, int, TypeMap);
        <<"float">> ->
            maps:put(Col, float, TypeMap);
        <<"string">> ->
            maps:put(Col, string, TypeMap);
        <<"json">> ->
            maps:put(Col, list, TypeMap);
        _ ->
            TypeMap
    end.

to_fields(FieldMap, TypeMap) ->
    maps:fold(
        fun(Col, Type, Acc) ->
            Field = maps:get(Col, FieldMap),
            Name = to_list(Field),
            [#table_field{col = Col, type = Type, name = Name} | Acc]
        end, [], TypeMap).

gen_record_value_str(Types, RowDataMap) ->
    lists:foldr(
        fun(#table_field{col = Col, name = Name, type = Type}, Acc) ->
            case maps:get(Col, RowDataMap, null) of
                null ->
                    Acc;
                Value ->
                    Str = to_str(Type, Value),
                    NewStr = to_number_str(Type, Str),
                    [Name ++ " => " ++ NewStr | Acc]
            end
        end, [], Types).

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
    set_cache(?ETS_MOD_STATE, list_to_atom(Mod), State).

del_mod_state(Mod) ->
    del_cache(?ETS_MOD_STATE, list_to_atom(Mod)).

get_sheet_state(Mod, Index) ->
    get_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}, ?LOADING).

set_sheet_state(Mod, Index, State) ->
    set_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}, State).

del_sheet_state(Mod, Index) ->
    del_cache(?ETS_SHEET_STATE, {list_to_atom(Mod), Index}).

get_sheet_key_list(Mod, Index) ->
    get_cache(?ETS_SHEET_KEY_LIST, {list_to_atom(Mod), Index}, []).

set_sheet_key_list(Mod, Index, KeyList) ->
    set_cache(?ETS_SHEET_KEY_LIST, {list_to_atom(Mod), Index}, KeyList).

del_sheet_key_list(Mod, Index) ->
    del_cache(?ETS_SHEET_KEY_LIST, {list_to_atom(Mod), Index}).

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