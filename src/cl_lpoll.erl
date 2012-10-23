%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_lpoll).
-export([init/3, content_types_provided/2, to_json/2]).

-include("../include/clamorous.hrl").

init(_Trasp, _Req, Opts) ->
  % I need to pass option somewhow, so 
  % blame cowboy's REST for this :)
  put(lpoll, proplists:get_value(long, Opts, true)),
  {upgrade, protocol, cowboy_http_rest}.

content_types_provided(Req, State) ->
  Content = [
      {{<<"application">>, <<"json">>, []}, to_json},
      {{<<"text">>, <<"plain">>, []}, to_json}
      ],
  {ok, Req1} = cowboy_http_req:set_resp_header(
      <<"Access-Control-Allow-Origin">>, <<"*">>, Req),
  {Content, Req1, State}.

to_json(Req, PName) ->
  LPoll = get(lpoll),
  {QS,  Req1} = cowboy_http_req:qs_vals(Req),
  {Seq, Req2} = clamorous_app:get_seq(Req1),
  MF = cl_data:parse_qs_to_mf(QS), 
  clamorous:subscribe(Seq, MF),
  Items = recv(LPoll),
  Resp  = cl_data:encode(Items),
  {Resp, Req2, PName}.

recv(LPoll) ->
  After = if
    LPoll -> infinity;
    true  -> 1
  end,
  receive 
    {_Mod, history, over} ->
      recv(LPoll);
    {_Mod, new, M} ->
      [M|recv(false)];
    {_Mod, history, M} ->
      [M|recv(false)]
  after
    After -> []
  end.

