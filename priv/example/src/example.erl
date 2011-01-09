%% Copyright (c) 2009 Hypothetical Labs, Inc.

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

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
    %% Example storing callback module specific state
    %% This modifies the server state
    {ok, gen_nb_server:store_cb_state([], State)}.

handle_call({add_listener, IpAddr, Port}, _From, State) ->
    %% Example of getting callback module state
    %% Not used here, but just an example
    [] = gen_nb_server:get_cb_state(State),
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
