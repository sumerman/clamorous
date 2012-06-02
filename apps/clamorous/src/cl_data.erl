%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_data).

-record(data, {id, timestamp, match_fields, content}).
%% Data fields
-export([id/1, set_id/2, timestamp/1, match_fields/1, content/1]). 
%% Data interop
-export([keypos/0, new/2, new_from_json/1, encode/1]).
%% Helpers
-export([gen_timestamp/0, gen_filter/1, parse_plist_to_mf/1]).
%% Harbinger channel
-export([subscribe/0, subscribe/1, send/1]).

new(MF, C) ->
	#data{ 
		id=cl_idgen:get_id(),
		timestamp=gen_timestamp(),
		match_fields=MF,
		content=C
		}.

id(#data{ id=ID }) -> ID.
set_id(D, ID) -> D#data{ id=ID}.

timestamp(#data{ timestamp=T }) -> T.
match_fields(#data{ match_fields=MF }) -> MF.
content(#data{ content=C }) -> C.

keypos() -> #data.id.

new_from_json(D) ->
	{struct, _} = mochijson2:decode(D),
	PL  = mochijson2:decode(D, [{format, proplist}]),
	MFV = case clamorous:get_conf(match_fields) of
		undefined -> PL;
		[] -> PL;
		MFL when is_list(MFL) ->
			[E || {F,_V}=E <- PL, lists:member(F, MFL)]
	end,
	new(MFV, {json, D}).

encode(L) when is_list(L) ->
	mochijson2:encode([
			{json, encode(M)}
			|| M <- L]);

encode(#data{} = M) ->
	ID   = cl_data:id(M), 
	Cont = cl_data:content(M),
	[mochijson2:encode([
				{id,ID}, 
				{data,Cont}]), $\n].

gen_timestamp() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

gen_filter(MFF) ->
	fun(_C, M) ->
		MFs = cl_data:match_fields(M),
		lists:all(fun({K,V}) ->
					V1 = proplists:get_value(K,MFs),
					%io:fromat("~n~p;~p~n~n", [V,V1]),
					V =:= V1
			end, MFF)
	end.

parse_plist_to_mf(PL) ->
	[{K,parse_val(V)} || {K,V} <- PL].

parse_val(V) ->
	try mochijson2:decode(V, [{format, proplist}])
	catch _:_ -> V end.


topic() -> 
	?MODULE.
subscribe() ->
	harbinger:subscribe(topic()).
subscribe(F) ->
	harbinger:subscribe(topic(), F).
send(#data{} = D) ->
	harbinger:send(topic(), D);
send(_) ->
	erlang:error(badarg).

