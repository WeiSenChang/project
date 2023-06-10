%% coding: utf-8
-module(lib_timer).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    date_time/0,
    unix_time/0,
    unix_time/1,
    minute_second/0,
    hour_second/0,
    day_second/0,
    to_local_time/1,
    next_min_time/0,
    next_hour_time/0,
    next_zero_time/0,
    next_zero_tick/0,
    curr_zero_tick/0
]).

date_time() ->
    erlang:localtime().

unix_time() ->
    Date = erlang:localtime(),
    unix_time(Date).

unix_time(Date) ->
    {Day, {H, M, S}} = calendar:time_difference(start_date_time(), Date),
    Day * day_second() + H * hour_second() + M * minute_second() + S.

minute_second() ->
    60.

hour_second() ->
    3600.

day_second() ->
    86400.

to_local_time(Tick) ->
    Start = calendar:datetime_to_gregorian_seconds(start_date_time()),
    calendar:gregorian_seconds_to_datetime(Start + Tick).

next_min_time() ->
    {_H, _M, S} = erlang:time(),
    minute_second() - S.

next_hour_time() ->
    {_H, M, S} = erlang:time(),
    hour_second() - M * minute_second() - S.

next_zero_time() ->
    {H, M, S} = erlang:time(),
    day_second() - H * hour_second() - M * minute_second() - S.

next_zero_tick() ->
    unix_time() + next_zero_time().

curr_zero_tick() ->
    next_zero_tick() - day_second().

%% 内部接口
%%%%%%%%
%% 系统开启日期时间
start_date_time() ->
    {{1970, 1, 1}, {0, 0, 0}}.