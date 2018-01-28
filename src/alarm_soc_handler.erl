%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 22:39
%%%-------------------------------------------------------------------
-module(alarm_soc_handler).
-author("prw").
-behaviour(gen_event).
-include("alarm_config.hrl").
%%%===================================================================
%%% API
%%%===================================================================
-export([init/1, handle_event/2, handle_call/2, add_socket/1, remove_socket/1]).


add_socket(Socket)->
  ok = gen_event:call(?NOTI_EVENT, ?MODULE, {add_socket, Socket}).

remove_socket(Socket)->
  ok = gen_event:call(?NOTI_EVENT, ?MODULE, {remove_socket, Socket}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, []}.

handle_event(#noti{} = Noti, Sockets) ->
  send_to_all(Noti, Sockets),
  {ok, Sockets};

handle_event(Event, State) ->
  send_to_all(Event, State),
  {ok, State}.


handle_call({remove_socket, Socket}, State) ->
  {ok, ok, remove_socket(Socket,State)};

handle_call({add_socket, Socket}, State) ->
  {ok, ok, [Socket|State]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


send_to_all(_, []) -> ok;
send_to_all(Msg, [H|T]) -> gen_server:cast(H,{send, Msg}), send_to_all(Msg, T).


remove_socket(_, []) -> [];
remove_socket(Socket, [H|T]) when Socket == H -> T;
remove_socket(Socket, [H|T]) -> [H|remove_socket(Socket, T)].


