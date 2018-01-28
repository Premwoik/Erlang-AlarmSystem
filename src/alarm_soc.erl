%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2018 14:47
%%%-------------------------------------------------------------------
-module(alarm_soc).
-author("prw").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {socket, type}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Socket :: port()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Socket) ->
  gen_server:start_link(?MODULE, Socket, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init(Socket) ->
%%  process_flag(trap_exit, true),
  gen_server:cast(self(), accept),
  {ok, #state{socket = Socket, type = listener}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(accept, State = #state{socket = ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  alarm_soc_sup:start_socket(),
  alarm_soc_handler:add_socket(self()),
  send(AcceptSocket, "Hello", []),
  {noreply, State#state{socket = AcceptSocket, type = connected}};

handle_cast({send, Msg}, State) ->
  send(State#state.socket, "~p", [Msg]),
  {noreply, State};

handle_cast(Msg, State) ->
  io:write(Msg),
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({tcp, Socket, "quit" ++ _}, State) ->
  gen_tcp:close(Socket),
  alarm_soc_handler:remove_socket(Socket),
  {stop, normal, State};

handle_info({tcp, Socket, Msg}, State) ->
  case Msg of
    "sabotage " ++ Sensor -> alarm_inputs:sabotage_sensor(list_to_integer(Sensor)), {noreply, State};
    "activate " ++ Sensor -> alarm_inputs:activate_sensor(list_to_integer(Sensor)), {noreply, State};
    "restart" -> ok = alarmsys_sup:restart_inputs(), send(Socket, "~p", [inputs_restarted]), {noreply, State};
    Otherwise -> case convert_to_code(Otherwise) of
                   {ok, Code} -> send(Socket, "~p", [alarm_core:handle_code(Code)]), {noreply, State};
                   {error, _} -> send(Socket, "wrong code", []), {noreply, State}
                 end
  end;

handle_info({tcp_closed, Socket}, State) ->
  alarm_soc_handler:remove_socket(Socket),
  io:format("tcp_closed: ~p\n", [Socket]), {stop, normal, State};

handle_info({tcp_error, Socket, _}, State) ->
  io:format("tcp_error: ~p\n", [Socket]), {stop, normal, State};


handle_info(E, State) ->
  io:fwrite("unexpected: ~p~n", [E]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, #state{socket = Socket}) ->
  alarm_soc_handler:remove_socket(Socket),
  ok = gen_tcp:close(Socket).

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_to_code(X) ->
  try list_to_integer(X) of
    R -> {ok, R}
  catch
    _:_ -> {error, not_integer}
  end.

send(Socket, Str, Args) ->
  gen_tcp:send(Socket, io_lib:format(Str, Args)).