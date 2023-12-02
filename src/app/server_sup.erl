%% -*- coding: utf-8 -*-

-module(server_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/4, terminate_child/1, delete_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_one, intensity => 5, period => 30}, []}}.

start_child(PName, Mod, Restart, Args) ->
    Child = #{id => PName, start => {Mod, start_link, Args},
        restart => Restart, shutdown => 2000, type => worker, modules => [Mod]},
    supervisor:start_child(erlang:whereis(?MODULE), Child).

terminate_child(PName) ->
    supervisor:terminate_child(erlang:whereis(?MODULE), PName).

delete_child(PName) ->
    supervisor:delete_child(erlang:whereis(?MODULE), PName).
