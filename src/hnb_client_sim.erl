-module(hnb_client_sim).

-export([start_clients/3]).

start_clients(Host, Index, End) when Index =< End ->
    io:format("."),
    handle_client_start(hnb_client_sup:start_client(Host), Host, Index, End);
start_clients(_Host, _Index, _End) ->
    io:format("~n"),
    ok.

handle_client_start({ok, Client}, Host, Index, End) ->
    User = sim_user_name(Index),
    handle_auth(hnb_client:auth(Client, User), Client, User),
    start_clients(Host, Index + 1, End);
handle_client_start({error, Err}, Host, _Index, _End) ->
    e2_log:error({client_connect, Err, Host}),
    e2_log:info("Quitting start_clients because of errors").

sim_user_name(Index) ->
    "sim_" ++ integer_to_list(Index).

handle_auth(ok, _Client, _User) -> ok;
handle_auth({error, Err}, Client, User) ->
    hnb_client:stop(Client),
    e2_log:error({client_auth_error, Err, User}).
