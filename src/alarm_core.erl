%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2018 18:02
%%%-------------------------------------------------------------------
-module(alarm_core).
-author("prw").
-behaviour(gen_statem).
-include("alarm_config.hrl").
%%%===================================================================
%%% API
%%%===================================================================

%% gen_statem export
-export([init/1, callback_mode/0, start_link/1, stop/0]).
%% states export
-export([idle/3, watch/3, alarm_on/3]).
%% api export
-export([activate_input/1, handle_code/1, turn_off_alarm/0, sabotage_input/1]).

-type inputs() :: [integer()].
-record(zone, {name :: atom(), inputs :: inputs()}).
-record(code, {num :: integer(), state :: atom(), type :: atom(), params :: term()}).
-type codes() :: [#code{}].
-type zones() :: [#zone{}].
-record(mem, {zones :: zones(), active_zones :: [atom()], codes :: codes()}).


start_link(Memory) -> gen_statem:start_link({local, ?MODULE}, ?MODULE, Memory, []).

stop() -> gen_statem:stop(?MODULE).

activate_input(Number) -> gen_statem:cast(?MODULE, {active_input, Number}).

sabotage_input(Number) -> gen_statem:cast(?MODULE, {sabotage_input, Number}).

turn_off_alarm() -> gen_statem:call(?MODULE, turn_off).

handle_code(Code) ->
  gen_statem:call(?MODULE, {handle_code, Code}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init(Memory) ->
  {ok, idle, Memory}.

callback_mode() -> state_functions.


alarm_on(Type, {handle_code, Code}, Data) ->
  catch alarm_on(Type, get_action_for_state(Type, Code, alarm_on, Data), Data);

alarm_on({call, From}, {turn_off, _X}, Data) ->
  notify(#noti{state = idle, reason = alarm_turned_off}),
  NewData = {mem, Data#mem.zones, [], Data#mem.codes},
  {next_state, idle, NewData, [{reply, From, alarm_turned_off}]};

alarm_on(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



idle(Type, {handle_code, Code}, Data) ->
  catch idle(Type, get_action_for_state(Type, Code, idle, Data), Data);

idle({call, From}, {activate_zone, Zones}, Data) ->
  NewActiveZones = lists:append(Zones, Data#mem.active_zones),
  notify({noti, watch, {zones_activation, Zones}, {all_active_zones, NewActiveZones}}),
  {next_state, watch, update_active_zones(NewActiveZones, Data), [{reply, From, zones_activated}]};

idle(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



watch(Type, {handle_code, Code}, Data) ->
  catch watch(Type, get_action_for_state(Type, Code, watch, Data), Data);

watch(cast, {active_input, Number}, Data) ->
  case input_from_active_zone(Number, Data) of
    true ->
      notify(#noti{state = alarm_on, reason = {input_activation, Number}}),
      {next_state, alarm_on, Data};
    false -> repeat_state_and_data
  end;

watch({call, From}, {deactivate_zone, Zones}, Data) ->
  NewActiveZones = lists:filter(fun(Zone) -> not lists:member(Zone, Zones) end, Data#mem.active_zones),
  {NextState, More} = get_more_info(deactivate_zone, NewActiveZones),
  notify({noti, NextState, {zones_deactivation, Zones}, More}),
  {next_state, NextState, update_active_zones(NewActiveZones, Data), [{reply, From, zones_deactivated}]};


watch({call, From}, {toggle_zone, Zones}, Data = #mem{active_zones = AcZones}) ->
  case lists:any(fun(El) -> lists:any(fun(El2) -> El == El2 end, AcZones) end, Zones) of
    true ->
      watch({call, From}, {deactivate_zone, Zones}, Data);
    false ->
      {_, _, NewData, State} = idle({call, From}, {activate_zone, Zones}, Data),
      {keep_state, rm_dup_active_zones(NewData), State}
  end;

watch(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).


%% Handle events common to all states
handle_event({call, From}, get_codes, Data) ->
  {keep_state, Data, [{reply, From, Data#mem.codes}]};

handle_event(cast, {sabotage_input, Num}, Data) ->
  notify(#noti{state = alarm_on, reason = {sabotage_input, Num}}),
  {next_state, alarm_on, Data};

%% DEFAULT event_handler
handle_event(Type, _, Data) ->
  case Type of
    {call, From} -> {keep_state, Data, [{reply, From, cannot_handle}]};
    cast -> {keep_state, Data}
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_more_info(deactivate_zone, NewActiveZones) ->
  case length(NewActiveZones) of
    0 -> {idle, all_zones_deactivated};
    _Else -> {watch, {still_active_zones, NewActiveZones}}
  end.


rm_dup_active_zones(Data = #mem{active_zones = Ac}) ->
  #mem{codes = Data#mem.codes, zones = Data#mem.zones, active_zones = lists:usort(Ac)}.


notify(Body) ->
  gen_event:notify(?NOTI_EVENT, Body).


update_active_zones(Zones, Data) ->
  #mem{zones = Data#mem.zones, active_zones = Zones, codes = Data#mem.codes}.


get_action_for_state(Type, Code, State, Data) ->
  Actions = lists:filter(
    fun(C) ->
      (C#code.num =:= Code) and (C#code.state =:= State)
    end, Data#mem.codes),
  case length(Actions) > 0 of
    true -> Action = hd(Actions), {Action#code.type, Action#code.params};
    false -> throw(build_response(Type, wrong_code))
  end.


build_response(cast, _Response) ->
  {repeat_state_and_data};
build_response({call, From}, Response) ->
  {repeat_state_and_data, [{reply, From, Response}]}.


input_from_active_zone(Number, Data) ->
  ActiveZones = lists:filter(fun(Zone) -> lists:member(Zone#zone.name, Data#mem.active_zones) end, Data#mem.zones),
  Inputs = lists:flatmap(fun(Zone) -> Zone#zone.inputs end, ActiveZones),
  lists:member(Number, Inputs).

