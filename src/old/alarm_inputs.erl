%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2017 18:17
%%%-------------------------------------------------------------------
-module(alarm_inputs).
-author("prw").
-behaviour(gen_server).

%% API
-export([signal/2, remote_signal/1, input_signal/1, wireless_input_signal/1]).






%%%%%%%%%%%%%%%%%%
%%% EXTERNAL API
%%%%%%%%%%%%%%%%%%



signal(remote, Signal) ->
  spawn(?MODULE, remote_signal, [Signal]);

signal(input, Signal) ->
  spawn(?MODULE, input_signal, [Signal]);

signal(wireless_input, Signal) ->
  spawn(?MODULE, wireless_input_signal, [Signal]).


%%%%%%%%%%%%%%%%%%%%%%
%%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%

remote_signal({RemoteId, BtnPressed}) ->
  Action = alarm_memory:get_remote_action(RemoteId, BtnPressed, mem),
  alarm:remote_action(Action).

input_signal(Id) ->
  case alarm_memory:is_inputs_zone_active(Id, mem) of
      true-> alarm:turn_on_alarm({input, Id});
      false -> ok
end.

wireless_input_signal(Id) ->
  input_signal(Id).