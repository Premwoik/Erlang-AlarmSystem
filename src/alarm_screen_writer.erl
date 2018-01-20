%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 19:47
%%%-------------------------------------------------------------------
-module(alarm_screen_writer).
-author("prw").
-behaviour(gen_event).

%% API
-export([init/1, handle_event/2, handle_call/2]).


init(_InitArgs) ->
  {ok, []}.

handle_event(Event, State) ->
  io:format("log: ~p\n", [Event]),
  {ok, State}.

handle_call(Request, State) ->
  io:format("log: ~p\n", [Request]),
  {ok, ok, State}.