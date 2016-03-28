-module (lela).
-behaviour (application).

%% Application callbacks
-export ([start/0, stop/0]).
-export ([start/2, stop/1]).

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
