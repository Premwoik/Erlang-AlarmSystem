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
  ok = gen_event:call(?MANAGER, ?MODULE, {add_socket, Socket}).

remove_socket(Socket)->
  ok = gen_event:call(?MANAGER, ?MODULE, {remove_socket, Socket}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, []}.

handle_event({noti, _OldState, NewState, Reason, Desc} = Handle, Sockets) ->
  Str = lists:flatten(io_lib:format("State: ~p Reason: ~p Desc: ~p", [NewState, Reason, Desc])),
  send_to_all(Handle, Sockets),
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

make_string(Data) ->
  lists:flatten(parse_state(Data#noti.state) ++ " " ++
    parse_reason(Data#noti.reason) ++ " " ++ parse_desc(Data#noti.desc)).

parse_reason({zones_activation, Zones}) -> io_lib:format("Zones activated: ~p", [Zones]);
parse_reason({zones_deactivation, Zones}) -> io_lib:format("Zones deactivated: ~p", [Zones]);
parse_reason({input_activation, Number}) -> io_lib:format("Input activation: ~p", [Number]);
parse_reason(alarm_turned_off) -> "Alarm turned off";
parse_reason(_) -> "".

parse_state(watch) -> "";
parse_state(idle) -> "";
parse_state(alarm_on) -> "ALARM ON".

parse_desc({still_active, List}) -> io_lib:format("Still active zones: ~p", [List]);
parse_desc({all_active_zones, List}) -> io_lib:format("All active zones: ~p", [List]).

send_to_all(_, []) -> ok;
send_to_all(Msg, [H|T]) -> gen_server:cast(H,{send, Msg}), send_to_all(Msg, T).


remove_socket(_, []) -> [];
remove_socket(Socket, [H|T]) when Socket == H -> T;
remove_socket(Socket, [H|T]) -> [H|remove_socket(Socket, T)].


