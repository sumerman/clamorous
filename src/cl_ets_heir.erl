%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_ets_heir).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, new/2]).

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

new(Name, Opts) ->
  case get_table(Name) of
    {ok, T} -> {ok, T};
    {error, _Reason} ->
      T = ets:new(Name, [{heir, whereis(?SERVER), Name}|Opts]),
      {ok, T}
  end.

get_table(Name) ->
  MRef = erlang:monitor(process, ?SERVER),
  gen_server:cast(?SERVER, {get_table, self(), Name}),
  Res = receive
    {'ETS-TRANSFER', Tab, _HeirPid, _Name} ->
      {ok, Tab};
    {'DOWN', MRef, process, _Pid, Reason} ->
      {error, Reason};
    {error, Reason} ->
      {error, Reason}
  end,
  erlang:demonitor(MRef),
  Res.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  D = dict:new(),
  {ok, D}.

handle_call(Msg, From, State) ->
  error_logger:error_msg(
    "Unhandled call: ~p from ~p to ~p ignored.", 
    [Msg, From, ?SERVER]),
  {noreply, State, hibernate}.

handle_cast({get_table, From, NameKey}, State) when is_pid(From) ->
  case dict:find(NameKey, State) of
    {ok, T} -> 
      ets:give_away(T, From, NameKey);
    error   -> 
      From ! {error, notab}
  end,
  error_logger:info_msg(
    "Table with key [~p] gave away.",
    [NameKey]),
  State1 = dict:erase(NameKey, State),
  {noreply, State1, hibernate};

handle_cast(Msg, State) ->
  error_logger:error_msg(
    "Unhandled cast: ~p to ~p ignored.", 
    [Msg, ?SERVER]),
  {noreply, State, hibernate}.

handle_info({'ETS-TRANSFER', Tab, _, NameKey}, State) ->
  case dict:find(NameKey, State) of
    {ok, T1} when (T1 /= Tab) ->
      error_logger:warning_msg(
        "Heired the table with key [~p], "
        "which is already used "
        "to store another table.", 
        [NameKey]);
    _Else -> ok
  end,
  error_logger:info_msg(
    "Heired the table with key [~p].",
    [NameKey]),
  State1 = dict:store(NameKey, Tab, State),
  {noreply, State1, hibernate};

handle_info(Msg, State) ->
  error_logger:error_msg(
    "Unhandled info: ~p to ~p ignored.", 
    [Msg, ?SERVER]),
  {noreply, State, hibernate}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

