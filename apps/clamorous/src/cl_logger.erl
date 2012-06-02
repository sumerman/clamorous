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

-record(state, { tab, backend=ets, min_id=0 }).
-record(idx, {t,k,v}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, [], []).

select(MFs) -> select(0, MFs).
select(LastID, MFs) ->
	P1 = list_servers(pg2:get_local_members(group()), LastID),
	P2 = list_servers(pg2:get_members(group()), LastID),
	P3 = [pg2:get_closest_pid(group())],
	case P1 ++ P2 ++ P3 of
		[] -> {ok, []};
		[P|_] ->
			gen_server:call(P, {select, LastID, MFs})
	end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	pg2:create(group()),
	pg2:join(group(), self()),
	T = ets:new(?MODULE, [
			duplicate_bag,
			{keypos, #idx.k},
			{read_concurrency, true}]),
	harbinger:subscribe(cl_data:topic()),
	set_timer(),
	{ok, #state{ tab=T }}.

handle_call(min_stored, _From, State) ->
	R = {ok, State#state.min_id},
	{reply, R, State};

handle_call({select, LID, MFs}, _From, State) ->
	R = {ok, do_select(State, LID, MFs)},
	{reply, R, State};

handle_call(_Request, _From, State) ->
	{stop, badmsg, State}.

handle_cast(_Msg, State) ->
	{stop, badmsg, State}.

handle_info(cleanup_time, State) ->
	S1 = cleanup(State),
	set_timer(),
	{noreply, S1};

handle_info(?NOTIFICATION(_C, N), State) ->
	insert_object(State, N),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

insert_object(State, O) ->
	MF   = cl_data:match_fields(O),
	Cont =  #idx{t=cont, k=cl_data:id(O), v=O}, 
	Time =  #idx{t=time, k=cl_data:timestamp(O), v=cl_data:id(O)}, 
	Prop = [#idx{t=prop, k=KV, v=cl_data:id(O)} || KV <- MF],
	LRes = [Cont|[Time|Prop]],
	insert(State, LRes).

insert(#state{ backend=B, tab=T }, L) ->
	B:insert(T, L).

sel_id_by_mf(#state{ backend=B, tab=T }, LastID, {_K,_V}=MF) ->
	Expr = [{#idx{t=prop,k=MF,v='$1', _='_'},[{'>','$1',LastID}],['$1']}],
	B:select(T, Expr).

sel_id_newer_than(#state{ backend=B, tab=T }, ID) ->
	Expr = [{#idx{t=cont,k='$1', _='_'},[{'>','$1',ID}],['$1']}],
	B:select(T, Expr).

lookup(#state{ backend=B, tab=T }, Key) ->
	try B:lookup_element(T, Key, #idx.v)
	catch _:_ -> [] end.

set_timer() ->
	M = clamorous:get_conf(cleanup_interval),
	erlang:send_after(timer:minutes(M), self(), cleanup_time).

cleanup(#state{ backend=B, tab=T, min_id=PMin } = St) ->
	{H, M, S} = clamorous:get_conf(history_storage_time),
	Sec = (timer:hms(H, M, S) div timer:seconds(1)),
	Now = cl_data:gen_timestamp(),
	Old = Now - Sec,
	Agr = fun
		(#idx{t=time,k=Tm,v=ID},MI) when (Tm=<Old) ->
			if ID > MI -> ID; true -> MI end;
		(_, MI) -> MI
	end,
	Min = B:foldl(Agr, PMin, T),
	Exp = [
			{#idx{v='$1', _='_'},[{'>','$1',Min}],[true]},
			{#idx{t=cont, k='$1', _='_'},[{'>','$1',Min}],[true]}
			],
	B:select_delete(T, Exp),
	St#state{ min_id=Min }.

do_select(St, LastID, MFs) when is_list(MFs) ->
	Newer = gb_sets:from_list(sel_id_newer_than(St, LastID)),
	SList = [gb_sets:from_list(sel_id_by_mf(St, LastID, MF)) || MF <- MFs],
	IDs   = gb_sets:to_list(gb_sets:intersection([Newer|SList])),
	[V || ID <- IDs, V <- lookup(St, ID)];
do_select(St, LastID, MF) when is_tuple(MF) ->
	do_select(St, LastID, [MF]).

group() -> {?MODULE, group}.

list_servers(Pids, LastID) ->
	Resp = [{P,gen_server:call(P, min_stored)} || P <- Pids],
	[P || {P,{ok,Min}} <- Resp, Min =< LastID].

