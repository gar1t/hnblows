-module(hnb_app).

-behavior(e2_application).

-export([init/0]).

init() ->
    {ok, [users_spec(),
          client_cleanup_sup_spec(),
          client_handler_sup_spec(),
          server_spec()]}.

users_spec() -> hnb_users.

client_cleanup_sup_spec() ->
    {hnb_client_cleanup_sup, [supervisor]}.

client_handler_sup_spec() ->
    {hnb_client_handler_sup, [supervisor]}.

server_spec() ->
    {hnb_server, start_link, [8888]}.
