%% -*- coding: utf-8 -*-

-module(lib_test_cmd).

-include("common.hrl").
-include("db_table.hrl").

-include_lib("xmerl/include/xmerl.hrl").

-include("lib_types.erl").

%% API
-export([role_gm/1, role_gm/2, role_gm/3, role_gm/4, role_gm/5]).
-export([test/0]).

role_gm(Gm) ->
    role_gm(Gm, 0).
role_gm(Gm, Par1) ->
    role_gm(Gm, Par1, 0).
role_gm(Gm, Par1, Par2) ->
    role_gm(Gm, Par1, Par2, 0).
role_gm(Gm, Par1, Par2, Par3) ->
    role_gm(Gm, Par1, Par2, Par3, 0).
role_gm("test_time", Secs = [_ | _], _Par2, _Par3, _Par4) ->
    NewSecs =
        case string:tokens(Secs, ",") of
            [Y,Mon,D,H,Min,S] ->
                NowTick = lib_time:to_unix_time(calendar:now_to_local_time(erlang:timestamp())),
                ToDataTime = {{lib_types:to_integer(Y), lib_types:to_integer(Mon), lib_types:to_integer(D)},
                    {lib_types:to_integer(H), lib_types:to_integer(Min), lib_types:to_integer(S)}},
                ToTick = lib_time:to_unix_time(ToDataTime),
                ToTick - NowTick;
            _ ->
                0
        end,
    lib_cache:set_test_secs(NewSecs),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;
role_gm("test_time", Secs, _Par2, _Par3, _Par4) ->
    lib_cache:set_test_secs(lib_types:to_integer(Secs)),
    gen_server:cast(time_server:get_pid(), restart_timer),
    ok;

role_gm("create", Num, _, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    ?INFO("create role start"),
    NameMap = lib_cache:get_role_name_map(),
    RoleIds = maps:values(NameMap),
    create(lists:max([0 | RoleIds]), Num, 1),
    EndTick = lib_time:unix_time(),
    ?INFO("create role end, use time ~w s", [EndTick - StarTick]);
role_gm("change_name", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OnLineMap = lib_cache:get_online_role_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    change_name(maps:to_list(OnLineMap)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("change name end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_login", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    all_role_login(lib_db:all_keys(?DB_ROLE)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("role login end, use time ~w s", [EndTick - StarTick]);
role_gm("all_role_logout", _Par1, _Par2, _Par3, _Par4) ->
    StarTick = lib_time:unix_time(),
    OnLineMap = lib_cache:get_online_role_map(),
    ?DEBUG("~w", [map_size(OnLineMap)]),
    all_role_logout(maps:to_list(OnLineMap)),
    EndTick = lib_time:unix_time(),
    ?DEBUG("role logout end, use time ~w s", [EndTick - StarTick]);


role_gm(Gm, _Par1, _Par2, _Par3, _Par4) ->
    ?DEBUG("no role gm: ~w", [Gm]).


%% 内部函数
create(_Value, Num, Count) when Num < Count ->
    ok;
create(Value, Num, Count) ->
    Name = "wsc" ++ lib_types:to_list(Value + Count),
    case lib_login:create(Name) of
        {ok, RoleId} ->
            lib_login:login(RoleId);
        _Other ->
            ?DEBUG("role create fail, info: ~w", [_Other])
    end,
    create(Value, Num, Count + 1).

change_name([]) ->
    ok;
change_name([{RoleId, _} | Tail]) ->
    Name = "test" ++ lib_types:to_list(RoleId),
    gen_server:cast(role_server:get_pid(RoleId), {role_change_name, Name}),
    change_name(Tail).


all_role_login([]) ->
    ok;
all_role_login([RoleId | Tail]) ->
    lib_login:login(RoleId),
    all_role_login(Tail).

all_role_logout([]) ->
    ok;
all_role_logout([{RoleId, _} | Tail]) ->
    lib_login:logout(RoleId),
    all_role_logout(Tail).

test() ->
    case zip:unzip("./cumulate_recharge-累积充值配置.xlsx", [memory]) of
        {ok, FB} ->
            _Sheets = load(FB);
        _Error ->
            ?DEBUG("~w", [_Error])
    end.

load(FB) ->
    V = proplists:get_value("xl/sharedStrings.xml", FB, undefined),
    Share = clean_share(V),
    Amount = 1,
    Tables = load(FB, Share, Amount, []),
    lists:reverse(Tables).

load(FB, Share, Amount, Tables) ->
    SheetName = "sheet" ++ integer_to_list(Amount),
    SheetPath = "xl/worksheets/" ++ SheetName ++ ".xml",
    case proplists:get_value(SheetPath, FB, undefined) of
        undefined -> Tables;
        SheetXML ->
            Sheet = clean_sheet(SheetXML),
            Table = {SheetName, pack_table(Share, Sheet)},
            load(FB, Share, Amount+1, [Table | Tables])
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
    clean_sheet_v(T, V).

%% 表格没有内容，为 null
clean_sheet_v(T, #xmlElement{content = [R]}) ->
    clean_sheet_v1(T, list_to_integer(R#xmlText.value));
clean_sheet_v(_, _) -> null.

%% 表格数据为字符串，需要从 share 转换
clean_sheet_v1(#xmlAttribute{value = "s"}, V) -> {transform, V};
clean_sheet_v1(_, V) -> V.

pack_table(Share, Sheet) ->
    [pack_row(Row, Share) || Row <- Sheet].

pack_row(Row, Share) ->
    [pack_value(V, Share) || V <- Row].

pack_value({transform, V}, Share) -> lists:nth(V+1, Share);
pack_value(V, _Share) -> V.