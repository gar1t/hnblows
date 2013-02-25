-module(hnb_server).

-behavior(e2_task).

-export([start_link/0, start_link/1]).

-export([init/1, handle_task/1]).

-record(state, {lsock}).

-define(DEFAULT_PORT, 33301).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, [], [registered]).

start_link(Port) ->
    e2_task:start_link(?MODULE, Port).

init([]) ->
    init(default_server_port());
init(Port) ->
    LSock = listen(Port),
    {ok, init_state(LSock)}.

default_server_port() ->
    case application:get_env(server_port) of
        {ok, Port} -> Port;
        undefined -> ?DEFAULT_PORT
    end.

init_state(LSock) ->
    #state{lsock=LSock}.

listen(Port) ->
    Opts = [{reuseaddr, true}, {active, false}, binary],
    case gen_tcp:listen(Port, Opts) of
        {ok, LSock} -> LSock;
        {error, Err} -> error({listen, Err})
    end.

%%%===================================================================
%%% Handle accept / client dispatch
%%%===================================================================

handle_task(#state{lsock=LSock}=State) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} ->
            handle_connection(Sock, State);
        {error, emfile} ->
            e2_log:error("Accept error: not enough file handles"),
            {repeat, State};
        {error, Err} ->
            error({accept, Err})
    end.

handle_connection(Sock, State) ->
    case hnb_client_handler_sup:start_handler(Sock) of
        {ok, Handler} -> 
            handle_client_handler(Handler, Sock, State);
        {error, Err} ->
            handle_client_handler_error(Err, State)
    end.

handle_client_handler(Handler, Sock, State) ->
    gen_tcp:controlling_process(Sock, Handler),
    {repeat, State}.

handle_client_handler_error(Err, State) ->
    e2_log:error({client_handler, Err}),
    {repeat, State}.
