%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dec 2017 20:55
%%%-------------------------------------------------------------------
-module(manipulator).
-author("prw").
-behaviour(gen_event).

%%%===================================================================
%%% API
%%%===================================================================
-import(alarm_core, [handle_code/1]).
-export([start/0, init/1, handle_event/2, handle_call/2]).


init(InitArgs) ->
  erlang:error(not_implemented).

handle_event(Event, State) ->
  erlang:error(not_implemented).

handle_call(Request, State) ->
  erlang:error(not_implemented).


send_code(Code) -> io:format("Response: ~p\n", [handle_code(Code)]).

start() ->
  case io:fread("Gimme code:", "~d") of
    {ok, [X]} -> send_code(X), start();
    _Else -> finito
end.

