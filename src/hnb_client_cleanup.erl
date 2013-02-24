-module(hnb_client_cleanup).

-behavior(e2_service).

-export([start_link/1]).

-export([init/1, handle_msg/3]).

-record(state, {ref}).

start_link(Client) ->
    e2_service:start_link(?MODULE, Client).

init(Client) ->
    Ref = monitor_client(Client),
    {ok, init_state(Ref)}.

monitor_client(Client) ->
    erlang:monitor(process, Client).

init_state(MonitorRef) ->
    #state{ref=MonitorRef}.

handle_msg({'DOWN', Ref, process, Client, _Info}, _From, #state{ref=Ref}) ->
    hnb_users:client_exit(Client),
    {stop, normal}.
