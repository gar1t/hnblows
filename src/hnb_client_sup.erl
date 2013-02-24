-module(hnb_client_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_client/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hnb_client, [registered]).

start_client(Host) ->
    e2_task_supervisor:start_task(?MODULE, [Host]).
