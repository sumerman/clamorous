%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%
%%% @doc Logger interface.
%%% Each logger should participate in the pg2 group
%%% thus data may be selected from 
%%% the closest one which holds requested data.
%%%
%%% This module relies on the fact that IDs of cl_data
%%% are unique and ordered in time.
%%%-------------------------------------------------------------------

-module(cl_logger).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([select/1, select/2, reg_as_logger/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Select all objects with given match field's values
%% from closest location.
-spec select(cl_data:match_fields()) -> 
	{ok, [cl_data:cl_data()]} | {error, any()}.
select(MFs) -> select(undefined, MFs).

%% @doc Select all objects with given match field's values
%% that have been created later than given LastID 
%% from the most appropriate location.
-spec select(cl_data:idt()|undefined, cl_data:match_fields()) -> 
	{ok, [cl_data:cl_data()]} | {error, any()}.
select(LastID, MFs) ->
	P1 = list_servers(pg2:get_local_members(group()), LastID),
	P2 = list_servers(pg2:get_members(group()), undefined),
	P3 = [pg2:get_closest_pid(group())],
	P  = hd(P1 ++ P2 ++ P3),
	if
		is_pid(P) ->
			retrive(P, LastID, MFs);
		true ->
			{ok, []}
	end.

retrive(P, LastID, MFs) ->
	Local = (node(P) == node()),
	case gen_server:call(P, {select, Local, LastID, MFs}, infinity) of
		{ok, F} when is_function(F) ->
			{ok, F()};
		R -> R
	end.

-spec reg_as_logger() -> ok.
reg_as_logger() ->
	pg2:create(group()),
	pg2:join(group(), self()),
	cl_data:subscribe().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

group() -> {?MODULE, group}.

-spec list_servers([pid()]|term(), cl_data:idt()|undefined) -> [pid()].
list_servers(Pids, LastID) when is_list(Pids) ->
	F = fun(P) -> 
			{ok, M} = gen_server:call(P, min_stored, infinity),
			{M,P}
	end,
	Resp = lists:keysort(1, lists:map(F, Pids)),
	[P || {M,P} <- Resp, M =< LastID];
list_servers(_, _) -> [].

