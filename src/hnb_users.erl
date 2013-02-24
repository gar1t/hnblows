-module(hnb_users).

-behavior(e2_task).

-export([start_link/0,
         try_auth/1,
         try_auth/2,
         unauth/1,
         user_client/1,
         foreach_client/1,
         user_count/0,
         client_exit/1]).

-export([init/1, handle_task/1]).

-record(user, {name, pid}).

-define(TAB, hnb_users).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link() ->
    e2_task:start_link(?MODULE, [], [registered]).

init([]) ->
    init_ets(),
    {ok, []}.

init_ets() ->
    ets:new(?TAB, [named_table, set, public, {write_concurrency, true}]),
    ets:insert(?TAB, {user_count, 0}).

handle_task(State) ->
    {hibernate, State}.

%%%===================================================================
%%% try_auth
%%%===================================================================

try_auth(UserName) ->
    try_auth(UserName, self()).

try_auth(UserName, Pid) ->
    case ets:lookup(?TAB, {user, UserName}) of
        [] ->
            add_user(UserName, Pid);
        [{_, #user{pid=Existing}}] ->
            {error, {exists, Existing}}
    end.

%%%===================================================================
%%% add_user
%%%===================================================================

add_user(Name, Pid) ->
    ets:insert(?TAB, {{user, Name}, new_user(Name, Pid)}),
    ets:insert(?TAB, {{client, Pid}, Name}),
    ets:update_counter(?TAB, user_count, 1),
    ok.

new_user(Name, Pid) ->
    #user{name=Name, pid=Pid}.

%%%===================================================================
%%% unauth
%%%===================================================================

unauth(Name) ->
    case ets:lookup(?TAB, {user, Name}) of
        [{_, #user{pid=Client}}] ->
            cleanup_user(Name, Client);
        [] -> ok
    end.

user_client(User) ->
    case ets:lookup(?TAB, {user, User}) of
        [{_, #user{pid=Pid}}] -> {ok, Pid};
        [] -> error
    end.

%%%===================================================================
%%% foreach_client
%%%===================================================================

foreach_client(Fun) ->
    apply_client_fun(Fun, ets:first(?TAB)).

apply_client_fun(_Fun, '$end_of_table') -> ok;
apply_client_fun(Fun, Key) ->
    maybe_apply_client_fun(Fun, Key),
    apply_client_fun(Fun, ets:next(?TAB, Key)).

maybe_apply_client_fun(Fun, {user, _}=Key) ->
    case ets:lookup(?TAB, Key) of
        [{_, #user{pid=Client}}] -> Fun(Client);
        [] -> ok
    end;
maybe_apply_client_fun(_Fun, _Other) -> ignore.

%%%===================================================================
%%% user_count
%%%===================================================================

user_count() ->
    ets:lookup_element(?TAB, user_count, 2).

%%%===================================================================
%%% client_exit
%%%===================================================================

client_exit(Pid) ->
    case ets:lookup(?TAB, {client, Pid}) of
        [{_, UserName}] ->
            cleanup_user(UserName, Pid);
        [] ->
            ok
    end.

cleanup_user(UserName, Client) ->
    ets:update_counter(?TAB, user_count, -1),
    ets:delete(?TAB, {client, Client}),
    ets:delete(?TAB, {user, UserName}),
    ok.
