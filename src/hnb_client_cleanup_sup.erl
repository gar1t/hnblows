-module(hnb_client_cleanup_sup).

-bebavior(e2_task_supervisor).

-export([start_link/0, start_cleanup/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hnb_client_cleanup, [registered]).

start_cleanup(Client) ->
    e2_task_supervisor:start_task(?MODULE, [Client]).
