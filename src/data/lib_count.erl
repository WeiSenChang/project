%% -*- coding: utf-8 -*-

-module(lib_count).

-include("common.hrl").

%% API
-export([
    get_role_id/0
]).

-define(ROLE_ID, "role_id").

%% API
get_role_id() ->
    lib_db:find_count(?ROLE_ID).
