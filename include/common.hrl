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

-define(DEBUG(Format), mod_log:debug_msg(?MODULE, ?LINE, Format, [])).
-define(DEBUG(Format, Args), mod_log:debug_msg(?MODULE, ?LINE, Format, Args)).

-define(INFO(Format), mod_log:info_msg(?MODULE, ?LINE, Format, [])).
-define(INFO(Format, Args), mod_log:info_msg(?MODULE, ?LINE, Format, Args)).

-define(TRY_CATCH(Func, Ret), try Func catch _:_ -> Ret end).
-define(JUDGE_RETURN(True, Ret), if True -> skip; true -> throw({break, Ret}) end).

-endif.
