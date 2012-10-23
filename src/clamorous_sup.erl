%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(clamorous_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDW(I, Opts), {I, {I, start_link, Opts}, transient, 5000, worker, [I]}).
-define(CHILDS(I, Opts), {I, {I, start_link, Opts}, permanent, infinity, supervisor, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_) ->
	Spec = [
		?CHILDW(cl_metapub, []),
		?CHILDS(cl_subscription_sup, []),
		?CHILDW(cl_discover, []),
		?CHILDW(cl_ets_heir, []),
		?CHILDW(cl_ets_logger, [])
	],
    {ok, { {one_for_one, 5, 10}, Spec} }.

