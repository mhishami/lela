-module(rentaka_sup).

-behaviour(supervisor).
-include ("rentaka.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  %% our mongo pool
  %% get configs
  % {ok, [{PoolName, SizeArgs, WorkerArgs}]} = application:get_env(rentaka, pools),
  % PoolArgs = [{name, {local, PoolName}}, {worker_module, mc_worker}] ++ SizeArgs,
  % PoolSpecs = poolboy:child_spec(PoolName, PoolArgs, WorkerArgs),

  % Mongo = ?CHILD(mongo_worker, worker, [PoolName]),

  % {ok, { {one_for_one, 5, 10}, [Mongo, PoolSpecs]} }.

  {ok, Args} = application:get_env(rentaka, mongo),
  Mongo = ?CHILD(rentaka_repo, worker, [Args]),

  {ok, { {one_for_one, 5, 10}, [Mongo]} }.
