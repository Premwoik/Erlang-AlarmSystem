%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2018 22:03
%%%-------------------------------------------------------------------
-author("prw").

-define(NOTI_EVENT, events).
-define(PORT, 8091).
-define(LOGS, "src/logs").
-define(MEMORY_FILE, "src/memory").
-define(SENSORS_NUM, 20).
-record(noti, {state = null, reason = null, more = null}).