%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_bqueue).

-record(bq, { 
    queue  = queue:new() :: queue(),
    bound  = 1           :: pos_integer(),
    length = 0           :: non_neg_integer()
    }).

-opaque cl_bqueue() :: #bq{}.

-export([
  new/1, 
  push/2, 
  oldest/1, 
  latest/1, 
  to_list/1
  ]).

-export_type([cl_bqueue/0]).

-spec new(pos_integer()) -> cl_bqueue().
new(Bound) when is_integer(Bound), (Bound > 0) ->
  #bq{ 
    queue  = queue:new(),
    bound  = Bound,
    length = 0
    };
new(_) -> 
  error(badarg).

-spec push(term(), cl_bqueue()) -> cl_bqueue().
push(E, #bq{ length=L, bound=B, queue=Q } = Bq) ->
  Q1 = queue:in_r(E, Q),
  case (L + 1) > B of
    true  -> 
      Bq#bq{ queue=queue:drop_r(Q1) };
    false ->
      Bq#bq{ queue=Q1, length = L + 1 }
  end.

-spec oldest(cl_bqueue()) -> term().
oldest(Bq) ->
  queue:get_r(Bq#bq.queue).

-spec latest(cl_bqueue()) -> term().
latest(Bq) ->
  queue:get(Bq#bq.queue).

-spec to_list(cl_bqueue()) -> list().
to_list(Bq) ->
  queue:to_list(Bq#bq.queue).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_bounded_test() ->
  K = 10,
  Q = lists:foldl(fun push/2, new(K), lists:seq(1, K+1)),
  K = length(to_list(Q)).

is_preempting_test() ->
  K = 10,
  Q = lists:foldl(fun push/2, new(K), lists:seq(1, K+1)),
  2 = oldest(Q).

is_order_correct_test() ->
  K = 10,
  B = 1, E = K,
  Q = lists:foldl(fun push/2, new(K), lists:seq(B, E)),
  B = oldest(Q),
  E = latest(Q).

age_in_list_asc_test() ->
  K = 10,
  Q = lists:foldl(fun push/2, new(K), lists:seq(1, K+K)),
  E = latest(Q),
  E = hd(to_list(Q)),
  B = oldest(Q),
  B = hd(lists:reverse(to_list(Q))).

-endif.

