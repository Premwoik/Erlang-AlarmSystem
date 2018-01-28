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

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init(_InitArgs) ->
  {ok, []}.

handle_event(Event, State) ->
  print(Event),
  {ok, State}.

handle_call(Request, State) ->
  print(Request),
  {ok, ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

print(Data) ->
  io:format(make_string(Data)).

make_string(Data) ->
  lists:flatten(get_date() ++ " LOG " ++ parse_to_string(Data) ++ "\n").

parse_to_string(Data) ->
  lists:flatten(io_lib:format("~p", [Data])).


get_date() ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
  io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Min, Sec]).