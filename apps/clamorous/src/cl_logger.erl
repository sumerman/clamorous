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

-record(state, {}).
-record(idx, {t,k,v}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

select(MFs) -> select(0, MFs).
select(LastID, MFs) when is_list(MFs) ->
	Newer = gb_sets:from_list(sel_id_newer_than(LastID)),
	SList = [gb_sets:from_list(sel_id_by_mf(LastID, MF)) || MF <- MFs],
	IDs   = gb_sets:to_list(gb_sets:intersection([Newer|SList])),
	[{ID,V} || ID <- IDs, V <- lookup(ID)];
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
			{keypos, #idx.k},
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
	insert_object(N),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

insert_object(O) ->
	MF   = cl_data:match_fields(O),
	Cont =  #idx{t=cont, k=cl_data:id(O), v=cl_data:content(O)}, 
	Time =  #idx{t=time, k=cl_data:timestamp(O), v=cl_data:id(O)}, 
	Prop = [#idx{t=prop, k=KV, v=cl_data:id(O)} || KV <- MF],
	LRes = [Cont|[Time|Prop]],
	insert(LRes).

delete_id(ID) ->
	ets:match_delete(?MODULE, #idx{v=ID, _='_'}),
	ets:delete(?MODULE, ID).

insert(L) ->
	ets:insert(?MODULE, L).

sel_id_by_mf(LastID, {_K,_V}=MF) ->
	Expr = [{#idx{t=prop,k=MF,v='$1', _='_'},[{'>','$1',LastID}],['$1']}],
	ets:select(?MODULE, Expr).

sel_id_newer_than(ID) ->
	Expr = [{#idx{t=cont,k='$1', _='_'},[{'>','$1',ID}],['$1']}],
	ets:select(?MODULE, Expr).

lookup(Key) ->
	try ets:lookup_element(?MODULE, Key, #idx.v)
	catch _:_ -> [] end.

set_timer() ->
	M = clamorous:get_conf(cleanup_interval),
	erlang:send_after(timer:minutes(M), self(), cleanup_time).

cleanup() ->
	{H, M, S} = clamorous:get_conf(history_storage_time),
	Sec = (timer:hms(H, M, S) div timer:seconds(1)),
	Now = cl_data:timestamp(cl_data:new([],[])),
	Old = Now - Sec,
	Exp = [{#idx{t=time,k='$1',v='$2', _='_'},[{'=<','$1',Old}],['$2']}],
	IDs = ets:select(?MODULE, Exp),
	[delete_id(ID) || ID <- IDs].


