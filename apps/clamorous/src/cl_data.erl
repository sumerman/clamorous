%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_data).

-record(data, {id, timestamp, match_fields, content}).
-export([id/1, set_id/2, timestamp/1, match_fields/1, content/1]). 
-export([keypos/0, topic/0, new/2, new_from_content/1, encode/1]).
-export([gen_timestamp/0, gen_filter/1]).

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

topic() -> ?MODULE.

new_from_content(D) ->
	{struct, PL} = mochijson2:decode(D),
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
					V1 =:= V
			end, MFF)
	end.	

