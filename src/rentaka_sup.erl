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
  {ok, Args} = application:get_env(rentaka, mongo),
  Mongo = ?CHILD(rentaka_repo, worker, [Args]),

  {ok, { {one_for_one, 5, 10}, [Mongo]} }.
