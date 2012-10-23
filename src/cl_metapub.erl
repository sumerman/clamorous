%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_metapub).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/clamorous.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send(M) ->
  case application:get_env(local_total_order_pub) of
    {ok, true} -> 
      gen_server:cast(?SERVER, {send, M});
    undefined  ->
      cl_data:send(M)
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  erlang:process_flag(priority, high),
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {stop, badmsg, State}.

handle_cast({send, M}, State) ->
  cl_data:set_id(M, cl_idgen:get_id()),
  cl_data:send(M),
  {noreply, State};

handle_cast(_Msg, State) ->
  {stop, badmsg, State}.

handle_info(_Info, State) ->
  {stop, badmsg, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

