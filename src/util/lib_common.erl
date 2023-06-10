%% coding: utf-8
-module(lib_common).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    tab/1
]).

%% 根据表名返回当前时间的新表名, 最小单位为1小时
tab(Tab) ->
    {{Year, Month, Day}, {Hour, _Min, _}} = lib_timer:to_local_time(lib_timer:unix_time()),
    TableName =
        erlang:atom_to_list(Tab) ++ "_" ++
        erlang:integer_to_list(Year) ++ "_" ++
        erlang:integer_to_list(Month) ++ "_" ++
        erlang:integer_to_list(Day) ++ "_" ++
        erlang:integer_to_list(Hour),
    erlang:list_to_atom(TableName).