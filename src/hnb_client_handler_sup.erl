-module(hnb_client_handler_sup).

-bebavior(e2_task_supervisor).

-export([start_link/0, start_handler/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hnb_client_handler, [registered]).

start_handler(Sock) ->
    e2_task_supervisor:start_task(?MODULE, [Sock]).
