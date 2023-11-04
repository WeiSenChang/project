%% coding: utf-8
-module(server_sup).
-author("weisenchang").

-behaviour(supervisor).

-include("common.hrl").

%% API
-export([start_link/0, start_child/4, delete_child/1, terminate_child/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ProcessName, Mod, RestartType, Args) ->
    AChild = #{id => ProcessName,
        start => {Mod, start_link, Args},
        restart => RestartType,
        shutdown => 2000,
        type => worker,
        modules => [Mod]},
    supervisor:start_child(erlang:whereis(?MODULE), AChild).

terminate_child(ProcessName) ->
    supervisor:terminate_child(erlang:whereis(?MODULE), ProcessName).

delete_child(ProcessName) ->
    supervisor:delete_child(erlang:whereis(?MODULE), ProcessName).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================