-module(hnb_client_handler).

-behavior(e2_service).

-export([start_link/1, tell/3]).

-export([init/1, handle_msg/3]).

-record(state, {sock, user}).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link(Sock) ->
    e2_service:start_link(?MODULE, Sock).

init(Sock) ->
    hnb_client_cleanup_sup:start_cleanup(self()),
    inet:setopts(Sock, [{active, once}]),
    {ok, init_state(Sock)}.

init_state(Sock) ->
    #state{sock=Sock, user=undefined}.

%%%===================================================================
%%% API
%%%===================================================================

tell(Client, From, Msg) ->
    e2_service:cast(Client, {tell, From, Msg}).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg({tcp, Sock, Data}, _From, #state{sock=Sock}=State) ->
    inet:setopts(Sock, [{active, once}]),
    handle_data(split_parts(Data), State);
handle_msg({tell, FromUser, Msg}, _From, State) ->
    send_msg(Msg, FromUser, State),
    {noreply, State};
handle_msg({tcp_closed, Sock}, _From, #state{sock=Sock}) ->
    {stop, normal}.

split_parts(Bin) ->
    binary:split(Bin, <<"\r\n">>, [global, trim]).

%%%===================================================================
%%% Command dispatch
%%%===================================================================

handle_data([], State) ->
    {noreply, State};
handle_data([Cmd|Rest], State) ->
    case handle_cmd(Cmd, State) of
        #state{}=NextState -> handle_data(Rest, NextState);
        _ -> handle_data(Rest, State)
    end.

handle_cmd(<<"tell ", More/binary>>, State) ->
    handle_tell(More, State);
handle_cmd(<<"yell ", More/binary>>, State) ->
    handle_yell(More, State);
handle_cmd(<<"auth ", More/binary>>, State) ->
    handle_auth(More, State);
handle_cmd(<<"whoami">>, State) ->
    handle_whoami(State);
handle_cmd(<<"user_count">>, State) ->
    handle_user_count(State);
handle_cmd(<<"quit\r\n">>, _State) ->
    exit(normal);
handle_cmd(_, State) ->
    reply({error, invalid_command}, State).

%%%===================================================================
%%% Command: tell
%%%===================================================================

handle_tell(_Data, #state{user=undefined}=State) ->
    reply({error, not_authenticated}, State);
handle_tell(Data, #state{user=_User}=State) ->
    handle_user_tell(parse_user_tell(Data), State).

parse_user_tell(Data) ->
     binary:split(Data, <<" ">>).

handle_user_tell([User, Msg], State) ->
    handle_user_client_tell(hnb_users:user_client(User), Msg, State);
handle_user_tell(_, State) ->
    reply({error, invalid_tell}, State).

handle_user_client_tell({ok, Client}, Msg, #state{user=From}=State) ->
    hnb_client_handler:tell(Client, From, Msg),
    reply(ok, State);
handle_user_client_tell(error, _Msg, State) ->
    reply({error, no_such_user}, State).

%%%===================================================================
%%% Command: yell
%%%===================================================================

handle_yell(_Msg, #state{user=undefined}=State) ->
    reply({error, not_authenticated}, State);
handle_yell(Msg, #state{user=User}=State) ->
    hnb_users:foreach_client(tell_user_fun(Msg, User)),
    reply(ok, State).

tell_user_fun(Msg, From) ->
    fun(Client) -> hnb_client_handler:tell(Client, From, Msg) end.

%%%===================================================================
%%% Command: auth
%%%===================================================================

handle_auth(Data, State) ->
    handle_user_from_data(user_from_data(Data), State).

user_from_data(<<"">>) -> {error, invalid_user};
user_from_data(<<"system">>) -> {error, invalid_user};
user_from_data(User) -> {ok, User}.

handle_user_from_data({ok, User}, State) ->
    handle_auth_user(User, State);
handle_user_from_data({error, Err}, State) ->
    reply({error, Err}, State). 

handle_auth_user(User, State) ->
    maybe_unauth(State),
    handle_try_auth(hnb_users:try_auth(User), User, State).

maybe_unauth(#state{user=undefined}) -> ok;
maybe_unauth(#state{user=User}) ->
    hnb_users:unauth(User).

handle_try_auth(ok, User, State) ->
    reply(ok, State),
    State#state{user=User};
handle_try_auth({error, {exists, _Client}}, _User, State) ->
    reply({error, user_exists}, State),
    State#state{user=undefined}.

%%%===================================================================
%%% Command: whoami
%%%===================================================================

handle_whoami(#state{user=undefined}=State) ->
    reply({error, not_authenticated}, State);
handle_whoami(#state{user=User}=State) ->
    reply(User, State).

%%%===================================================================
%%% Command: user_count
%%%===================================================================

handle_user_count(#state{user=undefined}=State) ->
    reply({error, not_authenticated}, State);
handle_user_count(State) ->
    Count = hnb_users:user_count(),
    reply(integer_to_list(Count), State).

%%%===================================================================
%%% Send msg
%%%===================================================================

send_msg(Msg, From, #state{sock=Sock}) ->
    Data = [<<"msg ">>, From, <<" ">>, Msg, <<"\r\n">>],
    handle_send_msg(gen_tcp:send(Sock, Data)).

handle_send_msg(ok) -> ok;
handle_send_msg({error, Err}) ->
    error({send_msg, Err}).

%%%===================================================================
%%% Util functions
%%%===================================================================

reply(ok, State) ->
    send_reply(<<"ok">>, State);
reply({error, Err}, State) ->
    ErrBin = format_bin(Err),
    send_reply(<<"error ", ErrBin/binary>>, State);
reply(Str, State) when is_binary(Str) orelse is_list(Str) ->
    send_reply([<<"ok ">>, Str], State).

format_bin(StrTerm) ->
    iolist_to_binary(io_lib:format("~s", [StrTerm])).

send_reply(Reply, #state{sock=Sock}) ->
    handle_send_reply(gen_tcp:send(Sock, [Reply, <<"\r\n">>])).

handle_send_reply(ok) -> ok;
handle_send_reply({error, Err}) ->
    error({send_reply, Err}).
