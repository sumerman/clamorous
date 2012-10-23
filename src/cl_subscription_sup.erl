%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_subscription_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_subscription/1, start_subscription/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDW(I, Opts), {I, {I, start_link, Opts}, temporary, brutal_kill, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_subscription(Opts) ->
  start_subscription(self(), Opts).

start_subscription(Client, Opts) ->
  supervisor:start_child(?MODULE, [Client, Opts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {simple_one_for_one, 5, 10}, [
        ?CHILDW(cl_subsription, [])
        ]} }.

