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

tables() ->
    [
        "role",
        "role_friend",
        "role_cache",

        "uid"
    ].

table("role") ->
    #table{name = "role", key = "id", def = "DB_ROLE", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "server_id", type = ?INT},
        #field{name = "server_name", type = ?STRING},
        #field{name = "account", type = ?STRING},
        #field{name = "item_map", type = ?INT, sub_type = "key_value"},
        #field{name = "item_list", type = ?LIST, sub_type = "key_value"}
    ]};
table("role_friend") ->
    #table{name = "role_friend", key = "id", def = "DB_ROLE_FRIEND", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "friend_map", type = ?MAP, sub_type = "friend"},
        #field{name = "apply_list", type = ?LIST, sub_type = ?INT},
        #field{name = "black_list", type = ?LIST, sub_type = ?INT}
    ]};
table("role_cache") ->
    #table{name = "role_cache", key = "id", def = "DB_ROLE_CACHE", type = ?TAB_TYPE_ROLE, fields = [
        #field{name = "id", type = ?INT},
        #field{name = "name", type = ?STRING},
        #field{name = "server_id", type = ?INT},
        #field{name = "server_name", type = ?STRING}
    ]};
table("uid") ->
    #table{name = "uid", key = "key", def = "DB_UID", type = ?TAB_TYPE_SYS, fields = [
        #field{name = "key", type = ?STRING},
        #field{name = "id", type = ?INT}
    ]};
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