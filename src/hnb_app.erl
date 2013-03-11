-module(hnb_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, child_specs()}.

child_specs() ->
    server_specs() ++ client_mode_specs().

server_specs() ->
    case servers_env() of
	[] -> [];
	Servers ->
            [users_spec(),
             client_cleanup_sup_spec(),
             client_handler_sup_spec()
	     |server_specs(Servers)]
    end.

servers_env() ->
    case application:get_env(servers) of
	{ok, Servers} -> Servers;
	undefined -> []
    end.

server_specs(Servers) ->
    [server_spec(Port) || Port <- Servers].

server_spec(Port) ->
    MFA = {hnb_server, start_link, [Port]},
    Opts = [{id, server_child_id(Port)}],
    {MFA, Opts}.

server_child_id(Port) -> {server, Port}.

client_mode_specs() ->
    case client_mode() of
        true -> [client_sup_spec()];
        false -> []
    end.

client_mode() ->
    case application:get_env(client_mode) of
        {ok, Val} -> Val;
        undefined -> false
    end.

users_spec() -> hnb_users.

client_cleanup_sup_spec() ->
    {hnb_client_cleanup_sup, [supervisor]}.

client_handler_sup_spec() ->
    {hnb_client_handler_sup, [supervisor]}.

client_sup_spec() ->
    {hnb_client_sup, [supervisor]}.
