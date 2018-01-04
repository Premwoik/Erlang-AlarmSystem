%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2017 19:26
%%%-------------------------------------------------------------------
-module(alarm_server).
-author("prw").
-behaviour(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, initialize/0, pass_code/2, get_status/1]).


init(Args) ->
  {ok, []}.
%%  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).





%%external api

initialize() ->
  gen_server:start_link({local, alarm_api}, ?MODULE, [],[]).

pass_code(Device, Code) ->
  erlang:error(not_implemented);

pass_code(Device, {Code, Action}) ->
  erlang:error(not_implemented).

get_status(Device) ->
  erlang:error(not_implemented).

