%% -*- coding: utf-8 -*-

-ifndef('common_HRL').
-define('common_HRL', true).

-define(DEBUG(Format), log_server:debug_msg(?MODULE, ?LINE, Format, [])).
-define(DEBUG(Format, Args), log_server:debug_msg(?MODULE, ?LINE, Format, Args)).

-define(INFO(Format), log_server:info_msg(?MODULE, ?LINE, Format, [])).
-define(INFO(Format, Args), log_server:info_msg(?MODULE, ?LINE, Format, Args)).

-define(WARING(Format), log_server:waring_msg(?MODULE, ?LINE, Format, [])).
-define(WARING(Format, Args), log_server:waring_msg(?MODULE, ?LINE, Format, Args)).

-define(ERROR(Format), log_server:error_msg(?MODULE, ?LINE, Format, [])).
-define(ERROR(Format, Args), log_server:error_msg(?MODULE, ?LINE, Format, Args)).

-define(IF(B, T, F), if B -> T; true -> F end).

-endif.
