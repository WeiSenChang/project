%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @author weisenchang
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2023 6:26
%%%-------------------------------------------------------------------
-author("weisenchang").

-ifndef('game_table_HRL').
-define('game_table_HRL', true).

-include("common.hrl").

%% db tables
tables() ->
    [
        "role",
        "role_friend",
        "role_cache",

        "uid"
    ].


%% db table
table("role") ->
    #table{name = "role", key = "id", def = "DB_ROLE", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT},
        #field{name = "exp", type = ?INT}
    ]};
table("role_friend") ->
    #table{name = "role_friend", key = "id", def = "DB_ROLE_FRIEND", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "friend_list", type = ?LIST, sub_type = "friend"},
        #field{name = "apply_list", type = ?LIST, sub_type = ?INT},
        #field{name = "black_list", type = ?LIST, sub_type = ?INT}
    ]};
table("role_cache") ->
    #table{name = "role_cache", key = "id", def = "DB_ROLE_CACHE", type = ?TAB_TYPE_SYS, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "level", type = ?INT},
        #field{name = "career", type = ?INT}
    ]};
table("uid") ->
    #table{name = "uid", key = "key", def = "DB_UID", type = ?TAB_TYPE_SYS, fields = [
        #field{name = "key", type = ?STRING},
        #field{name = "id", type = ?INT}
    ]};




%% db record
table("key_value") ->
    #table{name = "key_value", key = "key", fields = [
        #field{name = "key", type = ?INT},
        #field{name = "value", type = ?INT},
        #field{name = "other", type = ?LIST, sub_type = ?INT}
    ]};
table("friend") ->
    #table{name = "friend", key = "key", fields = [
        #field{name = "key", type = ?INT},
        #field{name = "value", type = ?STRING},
        #field{name = "other", type = ?LIST, sub_type = ?STRING}
    ]};
table(_) ->
    #table{}.




-endif.