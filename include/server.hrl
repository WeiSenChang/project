%% -*- coding: utf-8 -*-

-ifndef('server_HRL').
-define('server_HRL', true).

-define(SERVER_NO_START, 0).
-define(SERVER_STARTING, 1).
-define(SERVER_STARTED, 2).

-define(SERVER, server).

-define(SERVERS, [
    cache_server,
    log_server,
    time_server,
    role_manage_server
]).

-endif.