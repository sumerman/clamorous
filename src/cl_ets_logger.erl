%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%
%%% @doc ETS backend for logger interface
%%% @see cl_logger
%%%
%%% This module relies on the fact that IDs of cl_data
%%% are unique and ordered in time.
%%%-------------------------------------------------------------------

-module(cl_ets_logger).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/clamorous.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, force_cleanup/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, { tab, backend=ets, min_id, msg_cnt=0 }).
-record(idx, {t,k,v,ts,c}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local,?SERVER}, ?MODULE, [], []).

force_cleanup() ->
  ?SERVER ! cleanup_time.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  erlang:process_flag(priority, high),
  cl_logger:reg_as_logger(),
  %T = ets:new(?MODULE, [
  %duplicate_bag,
  %%protected,
  %{keypos, #idx.k},
  %{read_concurrency, true}]),
  {ok,T} = cl_ets_heir:new(?MODULE, [
        protected,
        duplicate_bag,
        {keypos, #idx.k},
        {read_concurrency, true}]),
  set_timer(),
  {ok, #state{ tab=T }}.

handle_call(min_stored, _From, State) ->
  R = {ok, State#state.min_id},
  {reply, R, State};

%% TODO there should be some concurrency restriction mechanism
%% to prevent 100+ simultaneous access attempts
%% probably pool of workers
handle_call({select, true, LID, MFs}, _From, State) ->
  R = {ok, fun() -> do_select(State, LID, MFs) end},
  {reply, R, State};

handle_call({select, _Local, LID, MFs}, _From, State) ->
  R = {ok, do_select(State, LID, MFs)},
  {reply, R, State, hibernate};

handle_call(_Request, _From, State) ->
  {stop, badmsg, State}.

handle_cast(_Msg, State) ->
  {stop, badmsg, State}.

handle_info(cleanup_time, State) ->
  S1 = cleanup(State),
  set_timer(),
  {noreply, S1};

handle_info(?CLDATA(N), State) ->
  S1 = insert_object(State, N),
  {noreply, S1}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec insert_object(#state{}, cl_data:cl_data()) -> #state{}.
insert_object(#state{ min_id=MID, msg_cnt=C } = State, O) ->
  ID   = cl_data:id(O),
  MF   = cl_data:match_fields(O),
  TS   = cl_data:timestamp(O),
  Cont =  #idx{t=cont, k=ID, v=O,  ts=TS, c=C}, 
  Prop = [#idx{t=prop, k=KV, v=ID, ts=TS, c=C} || KV <- MF],
  LRes = [Cont|Prop],
  insert(State, LRes),
  NID = min(ID, MID),
  State#state{ min_id=NID, msg_cnt=(C+1) }.

insert(#state{ backend=B, tab=T }, L) ->
  B:insert(T, L).

%% @doc Select the IDs of all objects with given match field's values
%% that have been created later than given LastID 
-spec sel_id_by_mf(#state{}, cl_data:idt(), cl_data:match_field()) -> 
  [cl_data:idt()].
sel_id_by_mf(#state{ backend=B, tab=T }, LastID, {_K,_V}=MF) ->
  Expr = [{#idx{t=prop,k=MF,v='$1', _='_'},
           [{'>','$1',LastID}],
           ['$1']}],
  B:select(T, Expr).

%% @doc Select the IDs of all objects created later than given LastID. 
-spec sel_id_newer_than(#state{}, cl_data:idt()) -> 
  [cl_data:idt()].
sel_id_newer_than(#state{ backend=B, tab=T }, ID) ->
  Expr = [{#idx{t=cont,k='$1', _='_'},
           [{'>','$1',ID}],
           ['$1']}],
  B:select(T, Expr).

lookup(#state{ backend=B, tab=T }, Key) ->
  try B:lookup_element(T, Key, #idx.v)
  catch _:_ -> [] end.

set_timer() ->
  M = clamorous:get_conf(cleanup_interval),
  erlang:send_after(timer:minutes(M), self(), cleanup_time).

-spec point_in_past() -> erlang:timestamp().
point_in_past() ->
  {H, M, S} = clamorous:get_conf(history_storage_time),
  Sec = (timer:hms(H, M, S) div timer:seconds(1)),
  Now = cl_data:gen_timestamp(),
  Now - Sec.

cleanup(#state{ backend=B, tab=T, msg_cnt=Cnt } = St) ->
  Old = point_in_past(),
  C   = Cnt - clamorous:get_conf(history_min_items),
  Exp = [{#idx{c='$1', ts='$2', _='_'},
           [{'<','$1',C}, {'<','$2',Old}],
           [true]}],
  %error_logger:info_msg("SEL:~p; C:~p Old:~p~n", [B:select(T,Exp),C,Old]),
  B:select_delete(T, Exp),
  % For now it's ok since used only in logger election heruistic,
  % but anyway TODO.
  St#state{ min_id=undefined }. 

%% @doc Select all objects with given match field's values
%% that have been created later than given LastID 
-spec do_select(#state{}, cl_data:idt(), cl_data:match_fields()) ->
  [cl_data:cl_data()].
do_select(St, LastID, []) ->
  IDs = sel_id_newer_than(St, LastID),
  get_objects(St, IDs);

do_select(St, LastID, MFs) when is_list(MFs) ->
  SList = [gb_sets:from_list(sel_id_by_mf(St, LastID, MF)) || MF <- MFs],
  IDs   = gb_sets:to_list(gb_sets:intersection(SList)),
  get_objects(St, IDs).

get_objects(St, IDs) when is_list(IDs) ->
  IDs1 = lists:usort(IDs),
  [V || ID <- IDs1, V <- lookup(St, ID)].

