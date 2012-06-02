%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_logger).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([select/1, select/2, reg_as_logger/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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

reg_as_logger() ->
	pg2:create(group()),
	pg2:join(group(), self()),
	harbinger:subscribe(cl_data:topic()).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

group() -> {?MODULE, group}.

list_servers(Pids, LastID) ->
	Resp = [{P,gen_server:call(P, min_stored)} || P <- Pids],
	[P || {P,{ok,Min}} <- Resp, Min =< LastID].

