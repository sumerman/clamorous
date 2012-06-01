%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_stream).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-include_lib("harbinger/include/harbinger.hrl").

gen_filter(MFF) ->
	fun(_C, M) ->
			MFs = cl_data:match_fields(M),
			lists:all(fun({K,V}) ->
						V1 = proplists:get_value(K,MFs),
						V1 =:= V
				end, MFF)
	end.	

init({_Any, http}, Req, []) ->
	{QS, Req1} = cowboy_http_req:qs_vals(Req),
	harbinger:subscribe(cl_data:topic(), gen_filter(QS)),
	{ok, Req1, QS}.

handle(Req, State) ->
	Headers    = [{'Content-Type', <<"text/event-stream">>}],
	{ok, Req2} = cowboy_http_req:chunked_reply(200, Headers, Req),
	Items      = cl_logger:select(0, State),
	[send_resp(Req2, I) || I <- Items],
	handle_loop(Req2, State).

handle_loop(Req, State) ->
	%{ok, Transport, Socket} = cowboy_http_req:transport(Req),
	receive
		?NOTIFICATION(_C, M) ->
			send_resp(Req, {
					cl_data:id(M), 
					cl_data:content(M)}),
			%Transport:send(Socket, Data),
			handle_loop(Req, State)
	end.

terminate(_Req, _State) ->
	ok.

send_resp(Req, {ID, Cont}) ->
	Data = [mochijson2:encode([
						{id,ID}, 
						{data,{json, Cont}}
						]), $\n],
	ok = cowboy_http_req:chunk(Data, Req).

