%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_send).

-export([init/3, allowed_methods/2, process_post/2,
		content_types_provided/2,
		post_is_create/2]).

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_http_rest}.

allowed_methods(Req, State) ->
	{['POST'], Req, State}.

content_types_provided(Req, State) ->
	Content = [
		{{<<"application">>, <<"json">>, []}, undef},
		{{<<"text">>, <<"plain">>, []}, undef}
	],
	{Content, Req, State}.

post_is_create(Req, State) ->
	{false, Req, State}.

process_post(Req, State) ->
	{ok, B, Req1} = cowboy_http_req:body(Req),
	R = try mochijson2:decode(B, [{format, proplist}]) of
		PL when is_list(PL) ->
			harbinger:send(clamorous, PL),
			[{status, ok}]
	catch
		error:E -> 
			ED = list_to_binary(io_lib:format("~p~n", [E])),
			[{status, error}, {description, ED}]

	end,
	Resp = mochijson2:encode(R),
	{ok, Req2} = cowboy_http_req:set_resp_body(Resp, Req1),
	{ok, Req3} = cowboy_http_req:set_resp_header(
			<<"Access-Control-Allow-Origin">>, <<"*">>, Req2),
	{true, Req3, State}.

