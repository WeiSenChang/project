%% coding: utf-8
-module(lib_timer).
-author("weisenchang").

-include("common.hrl").

%% API
-export([
    unix_time/0,
    to_unix_time/1,
    to_date_time/1,
    to_date_time/2,
    to_local_time/1,

    minute_second/0,
    hour_second/0,
    day_second/0,
    start_date_time/0,

    next_min_time/0,
    next_hour_time/0,
    next_zero_time/0,
    next_zero_tick/0,
    curr_zero_tick/0
]).

unix_time() ->
    to_unix_time(calendar:now_to_local_time(erlang:timestamp())) + ?TRY_CATCH(mod_timer:pass_secs(), 0).

to_unix_time(Date) ->
    calendar:datetime_to_gregorian_seconds(Date) - calendar:datetime_to_gregorian_seconds(start_date_time()).

to_date_time(Tick) ->
    to_date_time(Tick, 0).

to_date_time(Tick, X) ->
    NewTick = Tick + X * hour_second(),
    calendar:gregorian_seconds_to_datetime(NewTick + calendar:datetime_to_gregorian_seconds(start_date_time())).

to_local_time(Tick) ->
    to_date_time(Tick, 8).

next_min_time() ->
    {_, {_H, _M, S}} = to_local_time(unix_time()),
    minute_second() - S.

next_hour_time() ->
    {_, {_H, M, S}} = to_local_time(unix_time()),
    hour_second() - M * minute_second() - S.

next_zero_time() ->
    {_, {H, M, S}} = to_local_time(unix_time()),
    day_second() - H * hour_second() - M * minute_second() - S.

next_zero_tick() ->
    unix_time() + next_zero_time().

curr_zero_tick() ->
    next_zero_tick() - day_second().

%% 内部接口
%%%%%%%%
start_date_time() ->
    {{1970, 1, 1}, {0, 0, 0}}.

minute_second() ->
    60.

hour_second() ->
    3600.

day_second() ->
    86400.