-module(http_worker).

-behaviour(gen_server).

-define(RESPONSE, "HTTP/1.0 200 OK\r\nConnection: close\r\nContent-Length: 1\r\n\r\nA").
%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock}).

configure(Pid) ->
	gen_server:call(Pid, configure).

start(Sock) ->
	{ok, Pid} = gen_server:start(?MODULE, [Sock], []),
	gen_tcp:controlling_process(Sock, Pid),
	configure(Pid).

init([Sock]) ->
	{ok, #state{sock=Sock}}.


handle_call(configure, _From, #state{sock=Sock}=State) ->
	inet:setopts(Sock, [{active, once}, {packet, http}, binary]),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({http, Sock, http_eoh}, State) ->
	io:format("RESPONSE~n"),
	inet:setopts(Sock, [{packet, 0}]),
	gen_tcp:send(Sock, list_to_binary(?RESPONSE)),
	inet:setopts(Sock, [{packet, http}, {active, once}]),
	{noreply, State};

handle_info({http, Sock, {http_header, _, _, _, _}}, State) ->
	io:format("HEADER~n"),
	inet:setopts(Sock, [{active, once}]),
	{noreply, State};

handle_info({http, Sock, {http_request, _, _, _}}, State) ->
	io:format("REQUEST~n"),
	inet:setopts(Sock, [{active, once}]),
	{noreply, State};

handle_info({http, Sock, Data}, #state{sock=Sock}=State) ->
	io:format("Data: ~p~n", [Data]),
	gen_tcp:close(Sock),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal function
