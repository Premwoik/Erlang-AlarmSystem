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

%%%===================================================================
%%% API
%%%===================================================================

%% gen_statem export
-export([init/1, callback_mode/0, start/0, stop/0]).

-export([idle/3, watch/3, alarm_on/3]).

%%
-export([active_input/1, handle_code/1, turn_off_alarm/0, active_zone/1, check_state/0, handle_code2/1]).

-record(zone, {name, inputs = []}).
-record(code, {num, state, type, other}).
-record(mem, {zones = [], active_zones = [], codes = []}).

init(Args) ->
  {ok, idle, Args}.

callback_mode() -> state_functions.

%%%===================================================================
%%% External functions
%%%===================================================================


start() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, read_memory(), []).

stop() -> gen_statem:stop(?MODULE).

active_input(Number) -> gen_statem:cast(?MODULE, {active_input, Number}).

turn_off_alarm() -> gen_statem:call(?MODULE, turn_off).

active_zone(Names) -> gen_statem:call(?MODULE, {active_zone, Names}).

check_state() ->
  {State, _} = sys:get_state(?MODULE),
  State.

handle_code(Code) ->
  gen_statem:call(?MODULE, {handle_code, Code}).

handle_code2(Code) ->
  {State, Data} = sys:get_state(?MODULE),
  Actions = lists:filter(
    fun(C) ->
      (C#code.num =:= Code) and (C#code.state =:= State)
    end, Data#mem.codes),
  case length(Actions) > 0 of
    true -> Action = hd(Actions), gen_statem:call(?MODULE, {Action#code.type, Action#code.other});
    false -> wrong_code
  end.

get_name() -> ?MODULE.

%%%===================================================================
%%% States
%%%===================================================================


%%alarm_on(enter, _OldState, Data) -> {keep_state, Data};

alarm_on(Type, {handle_code, Code}, Data) ->
  catch alarm_on(Type, get_action_for_state(Type, Code, alarm_on, Data), Data);

alarm_on({call, From}, {turn_off, _X}, Data) ->
  {next_state, idle, Data, [{reply, From, alarm_turned_off}]};

alarm_on(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



idle(Type, {handle_code, Code}, Data) ->
  catch idle(Type, get_action_for_state(Type, Code, idle, Data), Data);

idle({call, From}, {active_zone, Zones}, Data) ->
  NewActiveZones = lists:append(Zones, Data#mem.active_zones),
  {next_state, watch, update_active_zones(NewActiveZones, Data), [{reply, From, zones_activated}]};

idle(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).



watch(Type, {handle_code, Code}, Data) ->
  catch watch(Type, get_action_for_state(Type, Code, watch, Data), Data);

watch(cast, {active_input, Number}, Data) ->
  case input_from_active_zone(Number, Data) of
    true -> {next_state, alarm_on, handle_alarm_turn_on(Data)};
    false -> repeat_state_and_data
  end;

watch({call, From}, {deactive_zone, Zones}, Data) ->
  NewActiveZones = lists:filter(fun(Zone) -> not lists:member(Zone, Zones) end, Data#mem.active_zones),
  case length(NewActiveZones) of
    0 ->
      {next_state, idle, update_active_zones(NewActiveZones, Data), [{reply, From, zones_deactivated}]};

    X when X < length(Data#mem.active_zones) ->
      {keep_state, update_active_zones(NewActiveZones, Data), [{reply, From, {still_active, NewActiveZones}}]};

    _Else -> {_, _, NewData, State} = idle({call, From}, {active_zone, Zones}, Data), {keep_state, NewData, State}
  end;

watch(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).


%% Handle events common to all states
handle_event({call, From}, get_codes, Data) ->
  {keep_state, Data, [{reply, From, Data#mem.codes}]};

handle_event(_, _, Data) ->
  {keep_state, Data}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


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

handle_alarm_turn_on(Data) ->
  %%notify_ppl
  %%turn_on_sarene
  io:format("ALARM!!!\n"), Data.

input_from_active_zone(Number, Data) ->
  ActiveZones = lists:filter(fun(Zone) -> lists:member(Zone#zone.name, Data#mem.active_zones) end, Data#mem.zones),
  Inputs = lists:flatmap(fun(Zone) -> Zone#zone.inputs end, ActiveZones),
  lists:member(Number, Inputs).

