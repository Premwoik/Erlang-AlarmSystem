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
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, start_link/0, stop/0, get_state/0]).


init({Port, Sockets}) ->
  start_server(Port),
  {ok, Sockets}.


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, {8091, []}, []).
stop() -> gen_server:stop(?MODULE).
get_state() -> sys:get_state(?MODULE).

handle_call({message, Message}, _From, State) ->
  X = binary_to_integer(Message),
  Result = alarm_core:handle_code(X),
  {reply, {reply, atom_to_binary(Result, utf8)}, State};

handle_call({send_event, Event}, _From, State) ->
  {reply, send_to_all(Event, State), State}.


handle_cast({remove_socket, Socket}, State) ->
  {noreply, remove_socket(Socket,State)};

handle_cast({add_socket, Socket}, State) ->
  {noreply, [Socket|State]}.







send_to_all(_, []) -> ok;
send_to_all(Msg, [H|T]) -> gen_tcp:send(H, Msg), send_to_all(Msg, T).

remove_socket(_, []) -> [];
remove_socket(Socket, [H|T]) when Socket == H -> T;
remove_socket(Socket, [H|T]) -> [H|remove_socket(Socket, T)].




start_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
    spawn(fun() -> acceptor(Listen) end).
acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> acceptor(ListenSocket) end),
  gen_server:cast(?MODULE, {add_socket, Socket}),
  handle(Socket).

%% Echoing back whatever was obtained
handle(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket),
      gen_server:cast(?MODULE, {remove_socket, Socket});
    {tcp, Socket, Msg} ->
      case  gen_server:call(?MODULE, {message, Msg}) of
        {reply, Reply} -> gen_tcp:send(Socket, Reply), handle(Socket);
        {noreply} -> handle(Socket)
      end
  end.