-module(hnb_client).

-behavior(e2_service).

-export([start_link/1,
         start_link/2,
         auth/2,
         whoami/1,
         tell/3,
         yell/2,
         user_count/1,
         stop/1]).

-export([init/1, handle_msg/3]).

-record(state, {sock, waiting}).

-define(TIMEOUT, 5000).
-define(DEFAULT_CLIENT_PORT, 33301).

%%%===================================================================
%%% Start / init
%%%===================================================================

start_link(Host) ->
    e2_service:start_link(?MODULE, Host).

start_link(Name, Host) ->
    e2_service:start_link(?MODULE, Host, [{registered, Name}]).

init(Host) ->
    Sock = connect(Host),
    {ok, init_state(Sock)}.

connect(HostSpec) ->
    {Host, Port} = connect_info(HostSpec),
    Opts = [{active, once}, binary],
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Sock} -> Sock;
        {error, Err} -> error({connect, Err})
    end.

connect_info({Host, Port}) -> {Host, Port};
connect_info(Host) -> {Host, default_host_port()}.

default_host_port() ->
    case application:get_env(default_client_port) of
        {ok, Port} -> Port;
        undefined -> ?DEFAULT_CLIENT_PORT
    end.

init_state(Sock) ->
    #state{sock=Sock,
           waiting=undefined}.

%%%===================================================================
%%% API
%%%===================================================================

auth(Client, Name) ->
    validate_user_name(Name),
    ok_val(e2_service:call(Client, {auth, Name})).

validate_user_name(Name) ->
    case re:run(Name, "^[a-zA-Z0-9_]+$$", [{capture, none}]) of
        match -> Name;
        nomatch -> error({badarg, Name})
    end.

whoami(Client) ->
    string_val(e2_service:call(Client, whoami)).

tell(Client, User, Msg) ->
    validate_user_name(User),
    ok_val(e2_service:call(Client, {tell, User, Msg})).

yell(Client, Msg) ->
    ok_val(e2_service:call(Client, {yell, Msg})).

user_count(Client) ->
    int_val(e2_service:call(Client, user_count)).

stop(Client) ->
    e2_service:call(Client, close).

%%%===================================================================
%%% Message dispatch
%%%===================================================================

handle_msg({tcp, Sock, Data}, _From, #state{sock=Sock}=State) ->
    inet:setopts(Sock, [{active, once}]),
    handle_data(split_parts(Data), State);
handle_msg({tell, User, Msg}, From, State) ->
    send_cmd([<<"tell ">>, User, <<" ">>, Msg], State),
    {noreply, schedule_timeout(From, State)};
handle_msg({yell, Msg}, From, State) ->
    send_cmd([<<"yell ">>, Msg], State),
    {noreply, schedule_timeout(From, State)};
handle_msg({auth, Name}, From, State) ->
    send_cmd([<<"auth ">>, Name], State),
    {noreply, schedule_timeout(From, State)};
handle_msg(whoami, From, State) ->
    send_cmd(<<"whoami">>, State),
    {noreply, schedule_timeout(From, State)};
handle_msg(user_count, From, State) ->
    send_cmd(<<"user_count">>, State),
    {noreply, schedule_timeout(From, State)};
handle_msg(timeout, _From, State) ->
    reply({error, timeout}, State),
    {noreply, clear_waiting(State)};
handle_msg(close, _From, State) ->
    {stop, normal, ok, State};
handle_msg({tcp_closed, Sock}, _From, #state{sock=Sock}) ->
    {stop, normal}.

split_parts(Bin) ->
    binary:split(Bin, <<"\r\n">>, [global, trim]).

%%%===================================================================
%%% Inbound data dispatch
%%%===================================================================

handle_data([], State) ->
    {noreply, State};
handle_data([Part|Rest], State) ->
    case handle_part(Part, State) of
        #state{}=NextState -> handle_data(Rest, NextState);
        _ -> handle_data(Rest, State)
    end.

handle_part(<<"msg ", More/binary>>, State) ->
    handle_user_msg(parse_user_msg(More), State);
handle_part(<<"ok">>, State) ->
    reply(ok, State);
handle_part(<<"ok ", Resp/binary>>, State) ->
    reply({ok, Resp}, State);
handle_part(<<"error ", Err/binary>>, State) ->
    reply({error, Err}, State);
handle_part(Other, _State) ->
    e2_log:error({unhandled_data, Other}).

parse_user_msg(Data) ->
     binary:split(Data, <<" ">>).

handle_user_msg([User, Msg], _State) ->
    io:format("~s said: ~s~n", [User, Msg]).

%%%===================================================================
%%% Misc functions
%%%===================================================================

send_cmd(Cmd, #state{sock=Sock}) ->
    handle_send_cmd(gen_tcp:send(Sock, [Cmd, "\r\n"])).

handle_send_cmd(ok) -> ok;
handle_send_cmd({error, Err}) ->
    error({send_cmd, Err}).

reply(Value, #state{waiting={TRef, Client}}=State) ->
    erlang:cancel_timer(TRef),
    e2_service:reply(Client, Value),
    clear_waiting(State).

schedule_timeout(Client, State) ->
    TRef = erlang:send_after(?TIMEOUT, self(), timeout),
    set_waiting({TRef, Client}, State).

set_waiting({_TRef, _Client}=Waiting, #state{waiting=undefined}=State) ->
    State#state{waiting=Waiting}.

clear_waiting(State) ->
    State#state{waiting=undefined}.

int_val({ok, Bin}) ->
    {ok, list_to_integer(binary_to_list(Bin))};
int_val({error, Err}) ->
    {error, format_err(Err)}.

string_val({ok, Bin}) ->
    {ok, binary_to_list(Bin)};
string_val({error, Err}) ->
    {error, format_err(Err)}.

ok_val(ok) -> ok;
ok_val({error, Err}) ->
    {error, format_err(Err)}.

format_err(<<"not_authenticated">>) -> not_authenticated;
format_err(<<"user_exists">>) -> user_exists;
format_err(<<"invalid_user">>) -> invalid_user;
format_err(<<"no_such_user">>) -> no_such_user;
format_err(Bin) when is_binary(Bin) -> binary_to_list(Bin);
format_err(Other) -> Other.
