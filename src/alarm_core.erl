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
-import(alarm_file, [read_memory/0, save_memory/0]).
-define(MANAGER, events).
-record(noti, {old_state = null, state = null, reason = null, desc = null}).
%%%===================================================================
%%% API
%%%===================================================================

%% gen_statem export
-export([init/1, callback_mode/0, start/0, stop/0]).

-export([idle/3, watch/3, alarm_on/3]).

%%
-export([active_input/1, handle_code/1, turn_off_alarm/0, active_zone/1, check_state/0, sabotage_input/1]).

-record(zone, {name, inputs = []}).
-record(code, {num, state, type, other}).
-record(mem, {zones = [], active_zones = [], codes = []}).

init(_Args) ->
  {ok, idle, read_memory()}.

callback_mode() -> state_functions.

%%%===================================================================
%%% External functions
%%%===================================================================


start() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_statem:stop(?MODULE).

active_input(Number) -> gen_statem:cast(?MODULE, {active_input, Number}).

sabotage_input(Number) -> gen_statem:cast(?MODULE, {sabotage_input, Number}).

turn_off_alarm() -> gen_statem:call(?MODULE, turn_off).

active_zone(Names) -> gen_statem:call(?MODULE, {active_zone, Names}).

check_state() ->
  {State, _} = sys:get_state(?MODULE),
  State.

handle_code(Code) ->
  gen_statem:call(?MODULE, {handle_code, Code}).

%%%===================================================================
%%% States
%%%===================================================================
%% TODO add removing active zones after alarm deactivation
alarm_on(Type, {handle_code, Code}, Data) ->
  catch alarm_on(Type, get_action_for_state(Type, Code, alarm_on, Data), Data);

alarm_on({call, From}, {turn_off, _X}, Data) ->
  notify(#noti{state = idle, reason = alarm_turned_off}),
  {next_state, idle, Data, [{reply, From, alarm_turned_off}]};

alarm_on(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



idle(Type, {handle_code, Code}, Data) ->
  catch idle(Type, get_action_for_state(Type, Code, idle, Data), Data);

idle({call, From}, {active_zone, Zones}, Data) ->
  NewActiveZones = lists:append(Zones, Data#mem.active_zones),
  notify(#noti{state = watch, reason = {zone_activation, Zones}, desc = {all_active_zones, NewActiveZones}}),
  {next_state, watch, update_active_zones(NewActiveZones, Data), [{reply, From, zones_activated}]};

idle(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



watch(Type, {handle_code, Code}, Data) ->
  catch watch(Type, get_action_for_state(Type, Code, watch, Data), Data);

watch(cast, {active_input, Number}, Data) ->
  case input_from_active_zone(Number, Data) of
    true ->
      notify(#noti{state = alarm_on, reason = {activate_input, Number}}),
      {next_state, alarm_on, Data};
    false -> repeat_state_and_data
  end;

watch({call, From}, {deactive_zone, Zones}, Data) ->
  NewActiveZones = lists:filter(fun(Zone) -> not lists:member(Zone, Zones) end, Data#mem.active_zones),
  case length(NewActiveZones) of
    0 ->
      notify(#noti{old_state = watch, state = idle, reason = {zones_deactivated, Zones}, desc = all_zones_deactivated}),
      {next_state, idle, update_active_zones(NewActiveZones, Data), [{reply, From, zones_deactivated}]};

    X when X < length(Data#mem.active_zones) ->
      notify(#noti{old_state = watch, state = watch,
        reason = {zones_deactivated, Zones}, desc = {still_active, NewActiveZones}}),
      {keep_state, update_active_zones(NewActiveZones, Data), [{reply, From, {still_active, NewActiveZones}}]};

    _Else -> {_, _, NewData, State} = idle({call, From}, {active_zone, Zones}, Data), {keep_state, NewData, State}
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

notify(Body) ->
  gen_event:notify(?MANAGER, Body).


update_active_zones(Zones, Data) ->
  #mem{zones = Data#mem.zones, active_zones = Zones, codes = Data#mem.codes}.


get_action_for_state(Type, Code, State, Data) ->
  Actions = lists:filter(
    fun(C) ->
      (C#code.num =:= Code) and (C#code.state =:= State)
    end, Data#mem.codes),
  case length(Actions) > 0 of
    true -> Action = hd(Actions), {Action#code.type, Action#code.other};
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


