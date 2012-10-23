%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_discover).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TIMEOUT, 1000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	P = clamorous:get_conf(discovery_port),
	E = clamorous:get_conf(discovery),
	S = if 
		E ->
			{ok, S1} = gen_udp:open(P, [
						binary,
						{active,true},
						{broadcast,true},
						{reuseaddr,true}]),
			timer:send_interval(?TIMEOUT, discovery_round),
			S1;
		true -> 
			undefined
	end,
	{ok, S}.

handle_call(_Request, _From, State) ->
	{stop, badmsg, State}.

handle_cast(_Msg, State) ->
	{stop, badmsg, State}.

handle_info({udp, _Sock, _IP, _Port, Data}, State) ->
	Node = (catch binary_to_term(Data)),
	catch net_kernel:connect_node(Node),
	{noreply, State};

handle_info(discovery_round, State) ->
	case {clamorous:get_conf(discovery), nodes()} of
		{true, []} ->
			send_info(State);
		_Else ->
			void
	end,
	{noreply, State};

handle_info(_Info, State) ->
	{stop, badmsg, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_info(Sock) ->
	Data = term_to_binary(node()),
	{ok, Port} = inet:port(Sock),
	gen_udp:send(Sock, {255,255,255,255}, Port, Data).


