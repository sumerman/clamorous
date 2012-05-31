-module(clamorous_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%TODO move to hrl
-define(PORT, 8080).
-define(ACCEPTORS_COUT, 100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	Disp = [
		{'_', [
				{[<<"clamorous">>, <<"send">>], cl_send, []}
				%{[<<"combiners">>, <<"list">>], is_combiners_list, []}
				%{[<<"combiners">>, <<"info">>, comb_name], is_combiner_info, []},
		]}
	],
	Env  = application:get_all_env(),
	Port = proplists:get_value(port, Env, ?PORT),
	cowboy:start_listener(introspec_http, ?ACCEPTORS_COUT,
		cowboy_tcp_transport, [{port, Port}],
		cowboy_http_protocol, [{dispatch, Disp}]
	),
    clamorous_sup:start_link().

stop(_State) ->
    ok.
