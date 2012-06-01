-module(clamorous_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(ACCEPTORS_COUT, 100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Disp = [
		{'_', [
				{[<<"clamorous">>, <<"publish">>], cl_send, []}
				%{[<<"combiners">>, <<"list">>], is_combiners_list, []}
				%{[<<"combiners">>, <<"info">>, comb_name], is_combiner_info, []},
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
