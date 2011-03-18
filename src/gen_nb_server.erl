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

-module(gen_nb_server).

-author('kevin@hypotheticalabs.com').

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
         get_cb_state/1,
         store_cb_state/2,
         add_listen_socket/2,
         remove_listen_socket/2]).

%% Behavior callbacks
-export([behaviour_info/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {cb,
                addrs=dict:new(),
                socks=dict:new(),
                server_state}).

%% @hidden
behaviour_info(callbacks) ->
    [{init, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2},
     {sock_opts, 0},
     {new_connection, 4}];

behaviour_info(_) ->
    undefined.

%% @doc Start server listening on IpAddr:Port
-spec start_link(atom(), [any()]) -> ok | ignore | {error, any()}.
start_link(CallbackModule, InitParams) ->
    gen_server:start_link(?MODULE, [CallbackModule, InitParams], [{fullsweep_after, 0}]).


%% @doc Start server listening on IpAddr:Port registered as Name
-spec start_link(atom(), atom(), [any()]) -> ok | ignore | {error, any()}.
start_link(Name, CallbackModule, InitParams) ->
    gen_server:start_link(Name, ?MODULE, [CallbackModule, InitParams], [{fullsweep_after, 0}]).

%% @doc Extracts the callback module's state from the server's overall state
%%      NOTE: Should only be called by the submodule
-spec get_cb_state(#state{}) -> any().
get_cb_state(#state{server_state=SState}) ->
    SState.

%% @doc Stores the callback module's state into the server's state
%%      NOTE: Should only be called by the submodule
-spec store_cb_state(any(), #state{}) -> #state{}.
store_cb_state(CBState, State) when is_record(State, state) ->
    State#state{server_state=CBState}.

%% @doc Adds a new listener socket to be managed by gen_nb_server
%%      NOTE: Should only be called by the submodule
-spec add_listen_socket({string(), integer()}, #state{}) -> {ok, #state{}} | {error, any()}.
add_listen_socket({IpAddr, Port}, #state{cb=Callback, addrs=Addrs, socks=Socks}=State) ->
    Key = {IpAddr, Port},
    case dict:find(Key, Socks) of
        {ok, _} ->
            {error, already_listening};
        error ->
            case listen_on(Callback, IpAddr, Port) of
                {ok, Sock} ->
                    {ok, State#state{socks=dict:store(Key, Sock, Socks),
                                     addrs=dict:store(Sock, Key, Addrs)}};
                Error ->
                    Error
            end
    end.

%% @doc Removes a new listener socket to be managed by gen_nb_server
%%      NOTE: Should only be called by the submodule
-spec remove_listen_socket({string(), integer()}, #state{}) -> {error, not_listening} | {ok, #state{}}.
remove_listen_socket({IpAddr, Port}, #state{socks=Socks, addrs=Addrs}=State) ->
    Key = {IpAddr, Port},
    case dict:find(Key, Socks) of
        error ->
            {error, not_listening};
        {ok, Sock} ->
            gen_tcp:close(Sock),
            {ok, State#state{socks=dict:erase(Key, Socks),
                             addrs=dict:erase(Sock, Addrs)}}
    end.

%% @doc Returns the callback module's state
-spec init([atom()|any()]) -> {ok, #state{}} | {error, bad_init_state} | {error, any()}.
init([CallbackModule, InitParams]) ->
    process_flag(trap_exit, true),
    State = #state{cb=CallbackModule},
    case CallbackModule:init(InitParams, State) of
        {ok, ServerState} when is_record(ServerState, state) ->
            {ok, ServerState};
        {ok, _State} ->
            {error, bad_init_state};
        Err ->
            Err
    end.

%% @hidden
handle_call(Request, From, #state{cb=Callback}=State) ->
    case Callback:handle_call(Request, From, State) of
        {reply, Reply, NewServerState} ->
            {reply, Reply, NewServerState};
        {reply, Reply, NewServerState, Arg} when Arg =:= hibernate orelse is_number(Arg) ->
            {reply, Reply, NewServerState, Arg};
        {noreply, NewServerState} ->
            {noreply, NewServerState};
        {noreply, NewServerState, Arg} when Arg =:= hibernate orelse is_number(Arg) ->
            {noreply, NewServerState, Arg};
        {stop, Reason, NewServerState} ->
            {stop, Reason, NewServerState};
        {stop, Reason, Reply, NewServerState} ->
            {stop, Reason, Reply, NewServerState}
    end.

%% @hidden
handle_cast(Msg, #state{cb=Callback}=State) ->
    case Callback:handle_cast(Msg, State) of
        {noreply, NewServerState} ->
            {noreply, NewServerState};
        {noreply, NewServerState, Arg} when Arg =:= hibernate orelse is_number(Arg) ->
            {noreply, NewServerState, Arg};
        {stop, Reason, NewServerState} ->
            {stop, Reason, NewServerState}
    end.

%% @hidden
handle_info({inet_async, ListSock, _Ref, {ok, CliSocket}}, #state{cb=Callback, addrs=Addrs}=State) ->
    inet_db:register_socket(CliSocket, inet_tcp),
    {IpAddr, Port} = dict:fetch(ListSock, Addrs),
    case Callback:new_connection(IpAddr, Port, CliSocket, State) of
        {ok, NewServerState} ->
            prim_inet:async_accept(ListSock, -1),
            {noreply, NewServerState};
        {stop, Reason, NewServerState} ->
            {stop, Reason, NewServerState}
    end;

handle_info(Info, #state{cb=Callback}=State) ->
    case Callback:handle_info(Info, State) of
        {noreply, NewServerState} ->
            {noreply, NewServerState};
        {noreply, NewServerState, Arg} when Arg =:= hibernate orelse is_number(Arg) ->
            {noreply, NewServerState, Arg};
        {stop, Reason, NewServerState} ->
            {stop, Reason, NewServerState}
    end.

%% @hidden
terminate(Reason, #state{cb=Callback, addrs=Addrs}=State) ->
    [gen_tcp:close(Sock) || Sock <- dict:fetch_keys(Addrs)],
    State1 = State#state{addrs=dict:new(), socks=dict:new()},
    Callback:terminate(Reason, State1),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% @hidden
%% @spec listen_on(CallbackModule, IpAddr, Port) -> Result
%%       CallbackModule = atom()
%%       IpAddr = string()
%%       Port = integer()
%%       Result = {ok, port()} | {error, any()}
listen_on(CallbackModule, IpAddr, Port) ->
    SockOpts = [{ip, convert(IpAddr)}|CallbackModule:sock_opts()],
    case gen_tcp:listen(Port, SockOpts) of
        {ok, LSock} ->
            {ok, _Ref} = prim_inet:async_accept(LSock, -1),
            {ok, LSock};
        Err ->
            Err
    end.

%% @hidden
%% @spec convert(Addr) -> Result
%%       Addr = string()
%%       Result = {integer(), integer(), integer(), integer()}
%% @doc Converts text IP addresses "0.0.0.0" to tuples {0, 0, 0, 0}
convert(Addr) ->
    T = string:tokens(Addr, "."),
    list_to_tuple([list_to_integer(X) || X <- T]).
