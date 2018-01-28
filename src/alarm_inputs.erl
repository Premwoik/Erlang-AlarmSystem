%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 11:47
%%%-------------------------------------------------------------------
-module(alarm_inputs).
-author("prw").
-behaviour(gen_server).

%%%===================================================================
%%% API
%%%===================================================================
-export([init/1, handle_call/3, handle_cast/2, stop/0, start_link/1]).

%%motion
-export([motion_sensor/0, get_sensors/0, get_module/0, spawn_sensors/2, sabotage_sensor/1, activate_sensor/1]).


sabotage_sensor(Number) ->
  Data = get_sensors(),
  Pid = get_sensor_by_number(Number, Data),
  exit(Pid, error).


activate_sensor(Number) ->
  Data = get_sensors(),
  Pid = get_sensor_by_number(Number, Data),
  erlang:send(Pid, move),
  ok.


get_sensors() -> gen_server:call(?MODULE, {get_sensors}).


start_link(Inputs) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Inputs, []).


stop() -> gen_server:stop(?MODULE).


get_module() -> ?MODULE.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Number) ->
  spawn_link(?MODULE, spawn_sensors, [self(), Number]),
  receive
    Sensors -> {ok, Sensors}
  end.


handle_call({get_sensors}, _From, State) ->
  {reply, State, State}.


handle_cast({sabotage, Pid}, State) ->
  Num = get_sensor_number(Pid, State, 1),
  alarm_core:sabotage_input(Num),
  {noreply, State};


handle_cast({move_detected, Pid}, State) ->
  Number = get_sensor_number(Pid, State, 1),
  alarm_core:activate_input(Number),
  {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

get_sensor_number(Pid, [H | T], Num) when Pid =:= H -> Num;
get_sensor_number(Pid, [_ | T], Num) -> get_sensor_number(Pid, T, Num + 1).


get_sensor_by_number(1, [H | _]) -> H;
get_sensor_by_number(Number, [_ | T]) -> get_sensor_by_number(Number - 1, T).

%% SENSOR SPAWNING AND MONITORING
sensors_monitor() ->
  receive
    ok -> ok;
    {'DOWN', _, _, Pid, error} -> gen_server:cast(?MODULE, {sabotage, Pid}), sensors_monitor()
  end.


spawn_sensors(Pid, Number) ->
  Pid ! [spawn_sensor() || _ <- lists:seq(1, Number)], sensors_monitor().


spawn_sensor() ->
  {Pid, _} = spawn_monitor(?MODULE, motion_sensor, []),
  Pid.

%%SENSOR
motion_sensor() ->
  receive
    move -> gen_server:cast(?MODULE, {move_detected, self()}), motion_sensor()
  end.

