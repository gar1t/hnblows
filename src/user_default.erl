-module(user_default).

-define(SERVER, shell_hnb_client).

-export([connect/2,
         auth/1,
         whoami/0,
         say/1,
         tell/2,
         yell/1,
         chat/0,
         chat/1,
         nochat/0,
         close/0,
         user_count/0]).

connect(Host, User) ->
    handle_connect_auth(connect(Host), User).

connect(Host) ->
    handle_connect(hnb_client:start_link(?SERVER, Host), Host).

handle_connect({ok, _Pid}, Host) ->
    put(last_connect, Host),
    ok;
handle_connect({error, {already_started, _Pid}}, _Host) ->
    already_connected;
handle_connect({error, Err}, _Host) ->
    {error, Err}.

handle_connect_auth(ok, User) -> auth(User);
handle_connect_auth(Other, _User) -> Other.

auth(User) ->
    handle_auth(try_client(auth, [User]), User).

handle_auth(ok, User) ->
    put(last_auth, User),
    ok;
handle_auth(Other, _User) ->
    Other.

whoami() ->
    try_client(whoami, []).

say(Msg) ->
    handle_chat_say(get(chat_user), Msg).

handle_chat_say(undefined, _Msg) -> no_chat;
handle_chat_say(User, Msg) -> tell(User, Msg).

tell(User, Msg) ->
    try_client(tell, [User, Msg]).

yell(Msg) ->
    try_client(yell, [Msg]).

close() ->
    handle_close(try_client(stop, [])).

handle_close(ok) ->
    erase(chat_user),
    erase(last_auth),
    erase(last_connect),
    ok;
handle_close(Other) ->
    Other.

chat() ->
    case get(chat_user) of
        undefined -> no_chat;
        User -> User
    end.

chat(User) ->
    put(chat_user, User),
    ok.

nochat() ->
    erase(chat_user),
    ok.

user_count() ->
    try_client(user_count, []).

try_client(Cmd, Args) ->
    try_client(Cmd, Args, auto_connect).

try_client(Cmd, Args, AutoConnect) ->
    try erlang:apply(hnb_client, Cmd, [?SERVER|Args]) of
        ok -> ok;
        {ok, Resp} -> Resp;
        {error, Err} -> Err;
        Other -> Other
    catch
        exit:{noproc, _} ->
            handle_not_connected(Cmd, Args, AutoConnect);
        T:E -> {T, E}
    end.

handle_not_connected(Cmd, Args, auto_connect) ->
    Last = {get(last_connect), get(last_auth)},
    maybe_auto_connect(Last, Cmd, Args);
handle_not_connected(_Cmd, _Args, no_auto_connect) ->
    not_connected.

maybe_auto_connect({Host, User}, Cmd, Args)
  when Host /= undefined andalso
       User /= undefined ->
    handle_auto_connect(connect(Host, User), Cmd, Args);
maybe_auto_connect(_Last, _Cmd, _Args) ->
    not_connected.

handle_auto_connect(ok, Cmd, Args) ->
    try_client(Cmd, Args, no_auto_connect);
handle_auto_connect(Other, _Cmd, _Args) ->
    Other.
