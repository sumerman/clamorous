-module(clamorous_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
%% Helper functions
-export([get_seq/1]).

-define(ACCEPTORS_COUT, 100).
-define(SEQ, seq).

%% ===================================================================
%% Application callbacks
%% ===================================================================

to_path(L) ->
	[atom_to_binary(I,latin1) || I <- L].

start(_StartType, _StartArgs) ->
	{ok, App} = application:get_application(),
	Disp = [
		{'_', [
				{to_path([App, publish]),                    cl_send,   []},
				{to_path([App, subscribe, stream]),          cl_stream, []},
				{to_path([App, subscribe, stream]) ++ [?SEQ], cl_stream, []}
		]}
	],
	Port = clamorous:get_conf(port),
	cowboy:start_listener(introspec_http, ?ACCEPTORS_COUT,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Disp}, {timeout, infinity}]
	),
    clamorous_sup:start_link().

stop(_State) ->
    ok.

get_seq(Req1) ->
	{Bin, Req2} = cowboy_http_req:binding(?SEQ, Req1),
	Seq = try list_to_integer(binary_to_list(Bin))
	catch _:_ -> new end,
	{Seq, Req2}.
