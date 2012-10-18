%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_stream).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2, handle_loop/2]).

init({_Any, http}, Req, []) ->
	{QS, Req1}  = cowboy_http_req:qs_vals(Req),
	{Seq, Req2} = clamorous_app:get_seq(Req1),
	MF = cl_data:parse_qs_to_mf(QS),
	clamorous:subscribe(Seq, MF),
	{ok, Req2, []}.

handle(Req, State) ->
	Headers = [
		{'Content-Type', <<"text/event-stream">>},
		{<<"Access-Control-Allow-Origin">>, <<"*">>}
	],
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
	{ok, T, S} = cowboy_http_req:transport(Req2),
	ok = T:setopts(S, [{active, true}]),
	handle_loop(Req2, State).

handle_loop(Req, State) ->
	receive
		{tcp,_Socket,_Data} ->
			{ok, Req, State};
		{tcp_closed,_Socket} ->
			{ok, Req, State};
		{_Mod, history, over} ->
			?MODULE:handle_loop(Req, State);
		{_Mod, _Src, M} ->
			send_resp(Req, M),
			?MODULE:handle_loop(Req, State)
	end.

terminate(_Req, _State) ->
	ok.

send_resp(Req, M) ->
	cowboy_http_req:chunk(cl_data:encode(M), Req),
	cl_data:id(M).


