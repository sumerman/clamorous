%%%-------------------------------------------------------------------
%%% @author Meleshkin Valery
%%% @copyright 2012 T-Platforms
%%%-------------------------------------------------------------------

-module(cl_data).

%% Data fields
-export([id/1, set_id/2, timestamp/1, match_fields/1, content/1]). 
%% Data interop
-export([keypos/0, new/2, new_from_json/1, new_from_plist/1, encode/1]).
%% Helpers
-export([gen_timestamp/0, gen_filter/1, parse_qs_to_mf/1]).
%% Harbinger channel
-export([subscribe/0, subscribe/1, send/1]).

-type idt()          :: non_neg_integer().
-type timestamp()    :: non_neg_integer().
-type match_field()  :: {any(), any()}.
-type match_fields() :: [match_field()].

-record(data, {
		id           = 0         :: idt(),
		timestamp    = 0         :: timestamp(), 
		match_fields = []        :: match_fields(), 
		content      = undefined :: any()
		}).

-opaque cl_data() :: #data{}.

-export_type([idt/0, timestamp/0, match_field/0, match_fields/0, cl_data/0]).

-spec new(match_fields(), any()) -> cl_data().
new(MF, C) ->
	MF1 = norm_plist(MF), 
	#data{ 
		id           = cl_idgen:get_id(),
		timestamp    = gen_timestamp(),
		match_fields = MF1,
		content      = C
		}.

-spec id(cl_data()) -> idt().
id(#data{ id=ID }) -> ID.

-spec set_id(cl_data(), idt()) -> cl_data().
set_id(D, ID) -> D#data{ id=ID}.


-spec timestamp(cl_data()) -> timestamp().
timestamp(#data{ timestamp=T }) -> T.

-spec match_fields(cl_data()) -> match_fields().
match_fields(#data{ match_fields=MF }) -> MF.

-spec content(cl_data()) -> any().
content(#data{ content=C }) -> C.

-spec keypos() -> non_neg_integer().
keypos() -> #data.id.

-spec norm_plist(proplists:proplist()) -> proplists:proplist().
norm_plist(PL) ->
	proplists:unfold(proplists:compact(PL)).

-spec extract_match_fields(proplists:proplist()) -> match_fields().
extract_match_fields(PL) ->
	case clamorous:get_conf(match_fields) of
		undefined -> PL;
		[] -> PL;
		MFL when is_list(MFL) ->
			MFL1 = lists:map(fun norm_mf_cfg_item/1, MFL),
			[E || {F,_V}=E <- PL, lists:member(F, MFL1)]
	end.

norm_mf_cfg_item(MFC) when is_atom(MFC) -> 
	atom_to_binary(MFC, utf8);
norm_mf_cfg_item(MFC) when is_list(MFC) -> 
	iolist_to_binary(MFC);
norm_mf_cfg_item(MFC) when is_binary(MFC) -> 
	MFC.

-spec new_from_plist(proplists:proplist()) -> cl_data().
new_from_plist(PL) ->
	PL1 = norm_plist(PL),
	MFV = extract_match_fields(PL1),
	DIO = mochijson2:encode(PL1),
	DBN = iolist_to_binary(DIO),
	new(MFV, {json, DBN}).

-spec new_from_json(binary()|iolist()) -> cl_data().
new_from_json(D) ->
	{struct, _} = mochijson2:decode(D),
	PL  = mochijson2:decode(D, [{format, proplist}]),
	MFV = extract_match_fields(PL),
	new(MFV, {json, D}).

-spec encode([cl_data()] | cl_data()) -> iolist().
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

-spec gen_timestamp() -> timestamp().
gen_timestamp() ->
	calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

-spec gen_filter(match_fields()) -> fun((_,any()) -> boolean()).
gen_filter(MFF) ->
	fun
		(_C, #data{} = M) ->
			MFs = cl_data:match_fields(M),
			lists:all(fun({K,V}) ->
						V1 = proplists:get_value(K,MFs),
						V =:= V1
				end, MFF);
		(_, _) -> false
	end.

-spec parse_qs_to_mf([{binary()|iolist(), binary()|iolist()}]) 
	-> match_fields().
parse_qs_to_mf(PL) ->
	[{K,parse_val(V)} || {K,V} <- PL].

-spec parse_val(binary()|iolist()) -> term().
parse_val(V) ->
	try mochijson2:decode(V, [{format, proplist}])
	catch _:_ -> V end.


topic() -> 
	?MODULE.
subscribe() ->
	harbinger:subscribe(topic()).
subscribe(F) ->
	harbinger:subscribe(topic(), F).

-spec send(cl_data()) -> true.
send(#data{} = D) ->
	harbinger:send(topic(), D);
send(_) ->
	erlang:error(badarg).

