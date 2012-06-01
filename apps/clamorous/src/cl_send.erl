%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_send).

-export([
	init/3, 
	allowed_methods/2, 
	malformed_request/2,
	process_post/2,
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

malformed_request(Req, State) ->
	{ok, B, Req1} = cowboy_http_req:body(Req),
	try cl_data:new_from_content(B) of
		M ->
			{false, Req1, M}
	catch
		error:E -> 
			{true, resp(Req1, error, E), State}
	end.

process_post(Req, M) ->
	harbinger:send(cl_data:topic(), M),
	{true, resp(Req, ok), M}.

resp(Req1, St) -> resp(Req1, St, done).
resp(Req1, St, Desc) ->
	Des1 = list_to_binary(io_lib:format("~p", [Desc])),
	Resp = [{status, St}, {description, Des1}],
	ReJS = mochijson2:encode(Resp),
	{ok, Req2} = cowboy_http_req:set_resp_body(ReJS, Req1),
	{ok, Req3} = cowboy_http_req:set_resp_header(
			<<"Access-Control-Allow-Origin">>, <<"*">>, Req2),
	Req3.

