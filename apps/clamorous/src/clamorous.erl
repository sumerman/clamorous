%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(clamorous).

-export([start/0, start/1, stop/0, get_conf/1]).
-export([send/2, send_plist/1, send_json/1, subscribe/2]).

%% Public API

start() ->
	start([]).

start(Opts) ->
	start_with_deps(?MODULE,  Opts).

stop() ->
    application:stop(?MODULE).

-spec send(cl_data:match_fields(), any()) -> true.
send(MatchFields, Data) when is_list(MatchFields) ->
	cl_data:send(cl_data:new(MatchFields, Data)).

-spec send_plist(proplists:proplist()) -> true.
send_plist(PL) ->
	cl_data:send(cl_data:new_from_plist(PL)).

-spec send_json(binary()|iolist()) -> true.
send_json(JSON) ->
	cl_data:send(cl_data:new_from_json(JSON)).

-spec subscribe(cl_data:idt(), cl_data:match_fields()) -> 
	{ok, pid()} | {error, term()}.
subscribe(LastID, MFs) ->
	cl_subscription_sup:start_subscription(self(), 
		[{last_id, LastID}, {match_fields, MFs}]).

%% Helper functions and Private API

start_with_deps(A, Opts) ->
	application:load(A),
	{ok, Ks} = application:get_all_key(A),
	Deps = proplists:get_value(applications, Ks, []),
	lists:foreach(fun ensure_started/1, Deps),
	[application:set_env(A, K, V) || {K, V} <- Opts],
    application:start(A).

ensure_started(A) ->
	case application:start(A) of
		ok -> ok;
		{error, {already_started, A}} -> ok
	end.

get_conf(K) ->
	% All defaults sould be in app.src!
	case application:get_env(?MODULE, K) of
		{ok, V} -> V;
		V -> V
	end.
