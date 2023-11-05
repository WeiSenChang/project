%% coding: utf-8
-module(lib_common).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    log_tab/1,
    rand_from_list/2,
    rand_from_list/3
]).

%% 根据表名返回当前时间的新表名, 最小单位为1小时
log_tab(Tab) ->
    {{Year, Month, Day}, {Hour, _Min, _}} = lib_timer:to_local_time(lib_timer:unix_time()),
    lib_types:to_list(Tab) ++ "_" ++ lib_types:to_list(Year) ++ "_" ++ lib_types:to_list(Month) ++ "_" ++
        lib_types:to_list(Day) ++ "_" ++ lib_types:to_list(Hour).

rand_from_list(List, Num) ->
    rand_from_list(List, Num, true).
rand_from_list(List, Num, true) ->
    Len = length(List),
    lists:foldl(
        fun(_, Acc) ->
            N = rand:uniform(Len),
            [lists:nth(N, List)|Acc]
        end, [], lists:seq(1, Num));
rand_from_list(List, Num, false) ->
    Len = length(List),
    {RandList, _NewList} = lists:foldl(
        fun(_, {AccList0, AccList1}) ->
            N = rand:uniform(Len),
            Term = lists:nth(N, List),
            {[Term|AccList0], lists:delete(Term, AccList1)}
        end, {[], List}, lists:seq(1, Num)),
    RandList;
rand_from_list(List, Num, _IsDup) ->
    rand_from_list(List, Num, false).