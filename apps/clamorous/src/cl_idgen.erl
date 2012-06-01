%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_idgen).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_id/0]).

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

get_id() ->
	ets:update_counter(?SERVER, id_counter, 1).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	ets:new(?SERVER, [
			set, public, named_table, 
			{write_concurrency, true}]),
	ets:insert(?SERVER, {id_counter, 0}),
	{ok, Args}.

handle_call(_Request, _From, State) ->
	{noreply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

