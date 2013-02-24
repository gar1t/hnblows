-module(hnb_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, child_specs()}.

child_specs() ->
    server_mode_child_specs() ++ client_mode_child_specs().

server_mode_child_specs() ->
    case server_mode() of
        true ->
            [users_spec(),
             client_cleanup_sup_spec(),
             client_handler_sup_spec(),
             server_spec()];
        false ->
            []
    end.

client_mode_child_specs() ->
    case client_mode() of
        true -> [client_sup_spec()];
        false -> []
    end.

server_mode() ->
    case application:get_env(server_mode) of
        {ok, Val} -> Val;
        undefined -> false
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

server_spec() ->
    {hnb_server, start_link, []}.

client_sup_spec() ->
    {hnb_client_sup, [supervisor]}.
