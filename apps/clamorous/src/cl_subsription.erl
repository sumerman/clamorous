%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_subsription).
-behaviour(gen_server).
-include("../include/clamorous.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {
		client=undefined,
		last_id=undefined, 
		match_fields=[],
		only_history=false
		}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Client, Args) ->
	gen_server:start_link(?MODULE, 
		[{client, Client} | Args], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
	Cli = proplists:get_value(client, Args),
	LID = proplists:get_value(last_id, Args),
	MFs = proplists:get_value(match_fields, Args, []),
	Old = proplists:get_bool(only_history, Args),
	erlang:monitor(process, Cli),
	not(Old) andalso 
		cl_data:subscribe(cl_data:gen_filter(MFs)),
	self() ! history,
	{ok, #state{
			client=Cli,
			last_id=LID,
			match_fields=MFs,
			only_history=Old
			}}.

handle_call(_Request, _From, State) ->
	{stop, badmsg, State}.

handle_cast(_Msg, State) ->
	{stop, badmsg, State}.

handle_info(history, #state{ last_id=LID, match_fields=MFs} = State) when is_integer(LID) ->
	{ok, Logs} = cl_logger:select(LID, MFs),
	LID1 = lists:foldl(fun(Msg, ID) ->
				send(history, State, Msg),
				max(ID, cl_data:id(Msg))
		end, 0, Logs),
	if
		State#state.only_history -> 
			{stop, normal, State};
		true ->
			{noreply, State#state{ last_id=LID1 }}
	end;

handle_info(history, State) ->
	error_logger:info_msg(
		"Subscription process ~p "
		"for client ~p supposed, that "
		"history isn't requested "
		"since given 'LastID' is ~p.",
		[self(), State#state.client, State#state.last_id]),
	{noreply, State};

handle_info(?CLDATA(M), #state{ last_id=LID } = State) ->
	ID = cl_data:id(M),
	% skip potential duplicates from history
	if
		ID =< LID -> nothing;
		true -> send(new, State, M)
	end,
	{noreply, State};

handle_info({'DOWN', _MRef, process, _Client, _Reason}, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{stop, badmsg, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send(Source, #state{ client=C }, Msg) ->
	C ! {?MODULE, Source, Msg}.

