%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2017 16:54
%%%-------------------------------------------------------------------
-module(alarm_memory).
-author("prw").
-behaviour(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, get_remote_action/3, is_inputs_zone_active/2]).
-record(user, {}).
-record(remote, {name::string(), user = #user{}, id::integer(), actions=#{}}).
-record(zone, {name::string(), inputs = [], is_active = false}).
-record(code, {code::integer(), action}).
-record(output, {id::integer(), state=low, name::string()}).
-record(mem, {outputs = [], remotes = [], zones = []}).

%%====================================================================
%% API
%%====================================================================

init(_Args) ->
  {ok, #mem{}}.


handle_call({get_zone, Which}, _From, State) ->
  case Which of
    all->
      {reply, State#mem.zones, State};
    _Else ->
        {reply, lists:filter(fun(Zone) ->
          lists:any(fun(InputId) -> InputId == Which end,Zone#zone.inputs) or Zone#zone.name == Which end,
        State#mem.zones)}
  end;

handle_call({get_output, Which}, _From, State) ->
  case Which of
      all-> {reply, State#mem.outputs, State};
      _Else -> {reply, lists:filter(
                        fun(Output) -> (Output#output.id == Which) or Output#output.name == Which end,
                        State#mem.outputs)}
end;

handle_call({get_remote, Which}, _From, State) ->
  case Which of
      all-> {reply, State#mem.remotes, State};
      _Else -> {reply, lists:filter(
                        fun(Remote) -> (Remote#output.id == Which) or Remote#output.name == Which end,
                        State#mem.remotes)}
end.


handle_cast(Request, State) ->
  erlang:error(not_implemented).

%%====================================================================
%% External API
%%====================================================================

get_output() -> 1.


-spec get_remote_action(string(), list(), term()) -> term().
get_remote_action(Name, BtnPressed, Server) ->
  Remotes = gen_server:call(Server, {get_remote, Name}),
  Remote = if
    length(Remotes) > 0 -> hd(Remotes);
    true -> throw({error,remote_with_given_name_dont_exist})
  end,
  case maps:find(BtnPressed, Remote#remote.actions) of
    {ok, Action} -> Action;
    error -> throw({error, no_action_for_pressed_buttons})
  end.

-spec is_inputs_zone_active(integer(), term()) -> boolean().
is_inputs_zone_active(Id, Server) ->
  Zones = gen_server:call(Server, {get_zone, Id}),
  lists:any(fun(Zone) -> Zone#zone.is_active == true end, Zones).

%%====================================================================
%% Internal functions
%%====================================================================
