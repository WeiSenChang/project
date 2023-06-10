%% coding: utf-8
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 5月 2023 2:57
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('common_HRL').
-define('common_HRL', true).

-define(LOG_LEVEL_DEBUG, 1).
-define(LOG_LEVEL_INFO, 2).
-define(LOG_LEVEL_WARNING, 3).
-define(LOG_LEVEL_ERROR, 4).

-define(DEBUG(Format), mod_log:msg(?MODULE, ?LINE, Format)).
-define(DEBUG(Format, Args), mod_log:msg(?MODULE, ?LINE, Format, Args)).

-define(INFO(Format), mod_log:msg(?MODULE, ?LINE, Format)).
-define(INFO(Format, Args), mod_log:msg(?MODULE, ?LINE, Format, Args)).

-define(WARNING(Format), mod_log:msg(?MODULE, ?LINE, Format)).
-define(WARNING(Format, Args), mod_log:msg(?MODULE, ?LINE, Format, Args)).

-define(ERROR(Format), mod_log:msg(?MODULE, ?LINE, Format)).
-define(ERROR(Format, Args), mod_log:msg(?MODULE, ?LINE, Format, Args)).

-define(TRY_CATCH(Func, Args), try apply(Func, Args) catch _:_Reason -> ?ERROR("~w", [_Reason]) end).
-define(TRY_CATCH(Mod, Name, Args), try apply(Mod, Name, Args) catch _:_Reason -> ?ERROR("~w", [_Reason]) end).


-record(key_value, {key, value}).

-endif.
