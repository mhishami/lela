-module (lela).
-behaviour (application).

%% Application callbacks
-export ([start/0, stop/0]).
-export ([start/2, stop/1]).
-export ([start_phase/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:ensure_all_started(lela).

stop() ->
  application:stop(lela).

start(_StartType, _StartArgs) ->
    lela_sup:start_link().

stop(_State) ->
    ok.

start_phase(start_listeners, _StartType, []) ->

  {ok, [{port, Port}, {listeners, Listeners}]} =
    application:get_env(lela, http),

  Dispatch = cowboy_router:compile([
    {'_', [
      {<<"/users">>, lela_users_handler, []},
      {<<"/users/[:id]">>, lela_users_update_handler, []}
    ]}
  ]),
  RanchOptions = [{port, Port}],
  CowboyOptions = [
    {env, [{dispatch, Dispatch}]},
    {compress, true},
    {timeout, 12000}
  ],
  cowboy:start_http(lela_http, Listeners, RanchOptions, CowboyOptions),
  ok.

