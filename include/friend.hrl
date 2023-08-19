%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 8月 2023 6:53
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('friend_HRL').
-define('friend_HRL', true).

-record(role_friend, {
    id = 0,
    friend_list = [],
    black_list = [],
    apply_list = [],
    req_add_list = []
}).

-endif.