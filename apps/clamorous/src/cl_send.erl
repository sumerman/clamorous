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
	Hook = [{object_hook, fun(X)->
				J = mochijson2:encode(X),
				cl_data:new_from_json(J) 
			end}],
	% single obj?
	try cl_data:new_from_json(B) of
		M -> {false, Req1, {one, M}}
	catch error:_ ->
			% may be array of objects?
			try mochijson2:decode(B, Hook) of
				M -> {false, Req1, {many, M}}
			catch error:E ->
				% no, just crap
				{true, resp(Req1, error, E), State}
			end
	end.

process_post(Req, {many, L}) when is_list(L) ->
	[cl_metapub:send(M) || M <- L], 
	ok(Req);
process_post(Req, {one, M}) ->
	cl_metapub:send(M),
	ok(Req).

ok(Req) -> {true, resp(Req, ok), ok}.

resp(Req1, St) -> resp(Req1, St, done).
resp(Req1, St, Desc) ->
	Des1 = list_to_binary(io_lib:format("~p", [Desc])),
	Resp = [{status, St}, {description, Des1}],
	ReJS = mochijson2:encode(Resp),
	{ok, Req2} = cowboy_http_req:set_resp_body(ReJS, Req1),
	{ok, Req3} = cowboy_http_req:set_resp_header(
			<<"Access-Control-Allow-Origin">>, <<"*">>, Req2),
	Req3.

