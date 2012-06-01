%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(clamorous).

-export([start/0, stop/0, get_conf/1]).

start() ->
	start_with_deps(?MODULE).

stop() ->
    application:stop(?MODULE).

start_with_deps(A) ->
	application:load(A),
	{ok, Ks} = application:get_all_key(A),
	Deps = proplists:get_value(applications, Ks, []),
	lists:foreach(fun ensure_started/1, Deps),
    application:start(A).

ensure_started(A) ->
	case application:start(A) of
		ok -> ok;
		{error, {already_started, A}} -> ok
	end.

get_conf(K) ->
	case application:get_env(?MODULE, K) of
		{ok, V} -> V;
		V -> V
	end.
