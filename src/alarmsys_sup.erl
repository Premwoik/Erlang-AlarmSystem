%%%-------------------------------------------------------------------
%% @doc alarmsys top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(alarmsys_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  Core = #{id => alarm_core,
    start => {alarm_core, start, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [alarm_core, alarm_file]},
  Inputs = #{id => alarm_inputs,
    start => {alarm_inputs, start, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [alarm_inputs]},

  ChildSpecs = [Core, Inputs],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
