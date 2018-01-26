%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jan 2018 22:03
%%%-------------------------------------------------------------------
-author("prw").

-define(MANAGER, events).
-define(PORT, 8091).
-define(LOGS, "src/logs").
-define(SENSOR_NUM, 20).
-record(noti, {old_state = null, state = null, reason = null, desc = null}).