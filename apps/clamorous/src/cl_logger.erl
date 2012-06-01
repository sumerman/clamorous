%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_logger).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("harbinger/include/harbinger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, select/1, select/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {next_id=0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

select(MFs) -> select(0, MFs).
select(LastID, MFs) when is_list(MFs) ->
	IDs = lists:flatten([sel_id_by_mf(LastID, MF) || MF <- MFs]),
	lists:flatten([lookup(ID) || ID <- lists:usort(IDs)]);
select(LastID, MF) when is_tuple(MF) ->
	select(LastID, [MF]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	ets:new(?MODULE, [
			duplicate_bag,
			protected,
			named_table, 
			{read_concurrency, true}]),
	harbinger:subscribe(cl_data:topic()),
	set_timer(),
	{ok, #state{}}.

handle_call(_Request, _From, State) ->
	{stop, badmsg, State}.

handle_cast(_Msg, State) ->
	{stop, badmsg, State}.

handle_info(cleanup_time, State) ->
	cleanup(),
	set_timer(),
	{noreply, State};

handle_info(?NOTIFICATION(_C, N), State) ->
	%error_logger:info_msg("Recvd ~p on ~p", [N, C]),
	insert_object(State#state.next_id, N),
	{noreply, incr_id(State)}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

insert_object(ID, D) ->
	O  = cl_data:set_id(D, ID),
	MF = cl_data:match_fields(O),
	insert({cl_data:id(O), O}), 
	insert({{time, cl_data:timestamp(O)}, cl_data:id(O)}), 
	[insert({KV, cl_data:id(O)}) || KV <- MF].

delete_id(ID) ->
	ets:match_delete(?MODULE, {'_', ID}),
	ets:delete(?MODULE, ID).

insert(L) ->
	ets:insert(?MODULE, L).

incr_id(#state{ next_id=NID } = State) ->
	State#state{ next_id=NID+1 }.

sel_id_by_mf(LastID, {_K,_V}=MF) ->
	Expr = [{{MF,'$1'},[{'>','$1',LastID}],['$1']}],
	ets:select(?MODULE, Expr).

lookup(Key) ->
	[V || {K,V} <- ets:lookup(?MODULE, Key), K == Key].

set_timer() ->
	M = clamorous:get_conf(cleanup_interval),
	erlang:send_after(timer:minutes(M), self(), cleanup_time).

cleanup() ->
	{H, M, S} = clamorous:get_conf(history_storage_time),
	Sec = (timer:hms(H, M, S) div timer:seconds(1)),
	Now = cl_data:timestamp(cl_data:new(0,[],[])),
	Old = Now - Sec,
	Exp = [{{{time,'$1'},'$2'},[{'=<','$1',Old}],['$2']}],
	IDs = ets:select(?MODULE, Exp),
	[delete_id(ID) || ID <- IDs].


