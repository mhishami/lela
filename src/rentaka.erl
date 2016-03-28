-module (rentaka).
-behaviour (application).

%% Application callbacks
-export ([start/0, stop/0]).
-export ([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:ensure_all_started(rentaka).

stop() ->
  application:stop(rentaka).

start(_StartType, _StartArgs) ->
    rentaka_sup:start_link().

stop(_State) ->
    ok.