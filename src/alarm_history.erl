%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 14:43
%%%-------------------------------------------------------------------
-module(alarm_history).
-author("prw").
-behaviour(gen_event).

%%%===================================================================
%%% API
%%%===================================================================

-export([init/1, handle_event/2, handle_call/2, save_to_file/2, make_string/1]).
-record(state, {file}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init(File) ->
  {ok, #state{file = File}}.

handle_event(Msg, State = #state{file = File}) ->
  save_to_file(Msg, File),
  {ok, State}.

handle_call(Msg, State = #state{file = File}) ->
  save_to_file(Msg, File),
  {ok, ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

save_to_file(Msg, File) ->
  case file:read_file_info(File) of
    {ok, _} ->
      file:write_file(File, make_string(Msg), [append]);
    {error, enoent} ->
      ok = file:write_file(File, make_string(Msg))
  end.

make_string(Msg) ->
  lists:flatten(get_date() ++ " :: " ++ parse_to_string(Msg)).

get_date() ->
  io_lib:format("~p", [calendar:local_time()]).

parse_to_string(Msg) ->
  lists:flatten(io_lib:format("~p", [Msg])) ++ "\n".
