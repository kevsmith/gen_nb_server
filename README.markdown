## Description
gen_nb_server is an OTP behavior designed to simplify writing completely non-blocking TCP servers.
Gone are the days of having to use a separate process to perform the listen/accept loop. Instead,
gen_nb_server uses features built into prim_inet to create a truly non-blocking server. This is the
same mechanism networking-intense projects, like RabbitMQ, use to create their TCP servers.

In addition, every gen_nb_server is also a gen_server so you can gen_server:call/cast/info to your
heart's content! What's not to like?

## How to use gen_nb_server

1. Drop the gen_nb_server behavior annotation at the top of your source file like so:
<pre>
-behavior(gen_nb_server).
</pre>

2. Implement the required functions. These include the usual suspects from gen_server (see the
[gen_server](http://www.erlang.org/doc/man/gen_server.html "gen_server manpage") manpage for details) and two new
functions: <code>sock_opts/0</code> and <code>new_connection/4</code>.

2a. <code>sock_opts/0</code> is used by gen_nb_server to retrieve the set of socket options to use when
creating the listen socket. These options will also be inherited by the client connection sockets. See the manpages
for [gen_tcp](http://www.erlang.org/doc/man/gen_tcp.html "gen_tcp manpage") and [inet](http://www.erlang.org/doc/man/inet.html "inet manpage") for more information on
socket options.

2b. <code>new_connection/4</code> is called every time a new connection is accepted. It is called with the newly
connected socket and the server's current state.

Here's a complete example which should give you an idea on how to use gen_nb_server:

<pre>
-module(example).

-export([start_link/0,
         add_listener/3,
         remove_listener/3]).

-export([init/2, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, sock_opts/0, new_connection/4]).

-behavior(gen_nb_server).

start_link() ->
    gen_nb_server:start_link(?MODULE, []).

add_listener(Pid, IpAddr, Port) ->
    gen_server:call(Pid, {add_listener, IpAddr, Port}).

remove_listener(Pid, IpAddr, Port) ->
    gen_server:call(Pid, {remove_listener, IpAddr, Port}).

init([], State) ->
    {ok, State}.

handle_call({add_listener, IpAddr, Port}, _From, State) ->
    case gen_nb_server:add_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
    end;
handle_call({remove_listener, IpAddr, Port}, _From, State) ->
    case gen_nb_server:remove_listen_socket({IpAddr, Port}, State) of
        {ok, State1} ->
            {reply, ok, State1};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State) ->
    Me = self(),
    P = spawn(fun() -> worker(Me, Sock, Data) end),
    gen_tcp:controlling_process(Sock, P),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

sock_opts() ->
    [binary, {active, once}, {packet, 0}].

new_connection(_IpAddr, _Port, Sock, State) ->
    Me = self(),
    P = spawn(fun() -> worker(Me, Sock) end),
    gen_tcp:controlling_process(Sock, P),
    {ok, State}.

worker(Owner, Sock) ->
    gen_tcp:send(Sock, "Hello\n"),
    inet:setopts(Sock, [{active, once}]),
    gen_tcp:controlling_process(Sock, Owner).

worker(Owner, Sock, Data) ->
    gen_tcp:send(Sock, Data),
    inet:setopts(Sock, [{active, once}]),
    gen_tcp:controlling_process(Sock, Owner).
</pre>

Note: This code is also available in priv/example.