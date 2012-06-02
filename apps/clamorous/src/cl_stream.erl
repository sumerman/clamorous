%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_stream).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2, handle_loop/2]).

-include_lib("harbinger/include/harbinger.hrl").
-record(state, {seq=0,mfs=[]}).

init({_Any, http}, Req, []) ->
	{QS, Req1}  = cowboy_http_req:qs_vals(Req),
	{Seq, Req2} = clamorous_app:get_seq(Req1),
	harbinger:subscribe(cl_data:topic(), cl_data:gen_filter(QS)),
	{ok, Req2, #state{seq=Seq, mfs=QS}}.

handle(Req, State) ->
	Headers    = [{'Content-Type', <<"text/event-stream">>}],
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
	handle_hist(Req2, State),
	handle_loop(Req2, State).

handle_hist(_Req, #state{seq=new} = State) -> State;
handle_hist(Req, #state{seq=N, mfs=MF} = State) when is_integer(N) ->
	{ok, Items} = cl_logger:select(N, MF),
	[send_resp(Req, I) || I <- Items],
	State.

handle_loop(Req, State) ->
	receive
		?NOTIFICATION(_C, M) ->
			send_resp(Req, M),
			?MODULE:handle_loop(Req, State)
	end.

terminate(_Req, _State) ->
	ok.

send_resp(Req, M) ->
	cowboy_http_req:chunk(cl_data:encode(M), Req).


