%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 22:39
%%%-------------------------------------------------------------------
-module(alarm_socket).
-author("prw").
-behaviour(gen_event).
-define(MANAGER, events).
%%-record(noti, {old_state = null, state = null, reason = null, desc = null}).
%%%===================================================================
%%% API
%%%===================================================================
-export([init/1, get_state/0, handle_event/2, handle_call/2]).


init({Port, Sockets}) ->
  start_server(Port),
  {ok, Sockets}.


get_state() -> sys:get_state(?MODULE).


handle_event({noti, _OldState, NewState, Reason, Desc}, Sockets) ->
  Str = lists:flatten(io_lib:format("NewState: ~p Reason: ~p Desc: ~p", [NewState, Reason, Desc])),
  send_to_all(Str, Sockets),
  {ok, Sockets};

handle_event(Event, State) ->
  send_to_all(Event, State),
  {ok, State}.



handle_call({remove_socket, Socket}, State) ->
  {ok, ok, remove_socket(Socket,State)};

handle_call({add_socket, Socket}, State) ->
  {ok, ok, [Socket|State]};

handle_call({send_event, Event}, State) ->
  {ok, send_to_all(Event, State), State};

handle_call({message, Message}, State) ->

  case binary_to_list(Message) of
      ""-> {ok, {noreply}, State};
      Otherwise -> case convert_to_code(Otherwise) of
             {ok, Code}-> Result = alarm_core:handle_code(Code), {ok, {reply, Result}, State};
             {error, _}-> {ok, {reply, wrong_command}, State}
           end
end.


%%%===================================================================
%%% Internal functions
%%%===================================================================



convert_to_code(X) ->
  try list_to_integer(X) of
    R -> {ok, R}
  catch
    _:_ -> {error, not_integer}
  end.

convert_to_binary(Result) when is_atom(Result)-> atom_to_binary(Result, utf8);
convert_to_binary(Result) when is_list(Result) -> list_to_binary(Result);
convert_to_binary(Result) when is_float(Result) -> float_to_binary(Result);
convert_to_binary(Result) when is_integer(Result) -> integer_to_binary(Result);
convert_to_binary(Result) -> term_to_binary(Result).



send_to_all(_, []) -> ok;
send_to_all(Msg, [H|T]) -> gen_tcp:send(H, convert_to_binary(Msg)), send_to_all(Msg, T).

remove_socket(_, []) -> [];
remove_socket(Socket, [H|T]) when Socket == H -> T;
remove_socket(Socket, [H|T]) -> [H|remove_socket(Socket, T)].


start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn(fun() -> acceptor(Listen) end).

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  ok = gen_event:call(?MANAGER, ?MODULE, {add_socket, Socket}),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      ok = gen_event:call(?MANAGER,?MODULE, {remove_socket, Socket});
    {tcp, Socket, Msg} ->
      case  gen_event:call(?MANAGER,?MODULE, {message, Msg}) of
        {reply, Reply} -> gen_tcp:send(Socket, convert_to_binary(Reply)), handle(Socket);
        {noreply} -> handle(Socket)
      end
  end.

