%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_stream).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2, handle_loop/2]).

-include_lib("harbinger/include/harbinger.hrl").
-record(state, {seq=0,mfs=[]}).

gen_filter(MFF) ->
	fun(_C, M) ->
			MFs = cl_data:match_fields(M),
			lists:all(fun({K,V}) ->
						V1 = proplists:get_value(K,MFs),
						V1 =:= V
				end, MFF)
	end.	

init({_Any, http}, Req, []) ->
	{QS, Req1}  = cowboy_http_req:qs_vals(Req),
	{Bin, Req2} = cowboy_http_req:binding(seq, Req1),
	harbinger:subscribe(cl_data:topic(), gen_filter(QS)),
	Seq = try list_to_integer(binary_to_list(Bin))
	catch _:_ -> new end,
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
			send_resp(Req, {
					cl_data:id(M), 
					cl_data:content(M)}),
			?MODULE:handle_loop(Req, State)
	end.

terminate(_Req, _State) ->
	ok.

send_resp(Req, {ID, Cont}) ->
	Data = [mochijson2:encode([
						{id,ID}, 
						{data,{json, Cont}}
						]), $\n],
	cowboy_http_req:chunk(Data, Req).

