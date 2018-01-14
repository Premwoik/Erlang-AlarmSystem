%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jan 2018 20:34
%%%-------------------------------------------------------------------
-module(alarm_file).
-author("prw").

%%%===================================================================
%%% API
%%%===================================================================
-export([read_memory/0, save_memory/0, read_file_to_string/0]).


read_memory() ->
  {ok, Tokens, _EndLine} = erl_scan:string(read_file_to_string()),
  {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
  {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.

save_memory() ->
  error(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================

read_file_to_string() ->
  {ok, File} = file:read_file("/home/prw/IdeaProjects/alarmsys/_build/default/lib/alarmsys/src/memory"),
  unicode:characters_to_list(File).