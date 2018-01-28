%%%-------------------------------------------------------------------
%% @doc alarmsys top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(alarmsys_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, restart_inputs/0]).

%% Supervisor callbacks
-export([init/1]).

-include("alarm_config.hrl").
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, read_memory()),
  ok = gen_event:add_handler(?NOTI_EVENT, alarm_soc_handler, []),
  ok = gen_event:add_handler(?NOTI_EVENT, alarm_history, [?LOGS]),
  ok = gen_event:add_handler(?NOTI_EVENT, alarm_screen_writer, []),
  {ok, Pid}.

restart_inputs() ->
  ok = supervisor:terminate_child(?SERVER, alarm_inputs),
  {ok, _} = supervisor:restart_child(?SERVER, alarm_inputs),
  ok.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Memory) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 60},
  Core = #{id => alarm_core,
    start => {alarm_core, start_link, [Memory]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [alarm_core, alarm_file]},
  Inputs = #{id => alarm_inputs,
    start => {alarm_inputs, start_link, [?SENSORS_NUM]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [alarm_inputs]},

  SocketsSup = #{id => socket_sup,
    start => {alarm_soc_sup, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [alarm_soc_sup]},
  Event = {my_event, {gen_event, start_link, [{local, ?NOTI_EVENT}]},
    permanent, 5000, worker, [alarm_soc_handler, alarm_core, alarm_history_handler]},

  ChildSpecs = [Inputs, Core, Event, SocketsSup],
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================


read_memory() ->
  {ok, Tokens, _EndLine} = erl_scan:string(read_file_to_string()),
  {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
  {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.

read_file_to_string() ->
  {ok, Path} = file:get_cwd(),
  {ok, File} = file:read_file(Path ++ "/src/memory"),
  unicode:characters_to_list(File).