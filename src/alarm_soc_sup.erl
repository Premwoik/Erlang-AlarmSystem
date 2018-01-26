%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2018 15:24
%%%-------------------------------------------------------------------
-module(alarm_soc_sup).
-author("prw").

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0, get_childes/0]).

%% Supervisor callbacks
-export([init/1]).
-include("alarm_config.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  {ok, ListenSocket} = gen_tcp:listen(?PORT, [{active,true}, {reuseaddr, true}]),

  Child = {alarm_soc, {alarm_soc, start_link, [ListenSocket]}, temporary, 1000, worker, [alarm_soc]},
  spawn(fun start_socket/0),
  {ok, { SupFlags, [Child]}}.


start_socket() ->
  supervisor:start_child(?MODULE, []).

get_childes() ->
  supervisor:which_children(?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================


