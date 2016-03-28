-module(lela_repo).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include ("lela.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export ([topology/0]).
-export ([find_one/2, find_one/3]).
-export ([find/2, find/3, find/4]).
-export ([save/2, save/3]).
-export ([update/2, update/3, update/4]).
-export ([delete/2, delete/3]).
-export ([ensure_index/2]).
-export ([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

-record (state, {topo = undefined}).

-define (POOL, lela).
-define (TIMEOUT, infinity).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

topology() ->
  gen_server:call(?MODULE, {topology}).

%% ============================================================================
% lela_repo:find_one(DB, #{ <<"email">> => <<"hisham@gmail.com">>}).
find_one(Collection, Selector) ->
  gen_server:call(?MODULE, {find_one, Collection, Selector, #{}}).

find_one(Collection, Selector, Projector) ->
  gen_server:call(?MODULE, {find_one, Collection, Selector, Projector}).

%% ============================================================================
find(Collection, Selector) ->
  gen_server:call(?MODULE, {find, Collection, Selector, #{}, ?TIMEOUT}).

find(Collection, Selector, Projector) ->
  gen_server:call(?MODULE, {find, Collection, Selector, Projector, ?TIMEOUT}).

find(Collection, Selector, Projector, Timeout) ->
  gen_server:call(?MODULE, {find, Collection, Selector, Projector, Timeout}).

%% ============================================================================
% lela_repo:save(DB, Model)
save(Collection, Doc) ->
  gen_server:call(?MODULE, {save, Collection, Doc, ?TIMEOUT}).

save(Collection, Doc, Timeout) ->
  gen_server:call(?MODULE, {save, Collection, Doc, Timeout}).

%% ============================================================================
update(Collection, Doc) ->
  gen_server:call(?MODULE, {update, Collection, #{}, Doc, #{}}).

update(Collection, Selector, Doc) ->
  gen_server:call(?MODULE, {update, Collection, Selector, Doc, #{}}).

update(Collection, Selector, Doc, Opts) ->
  gen_server:call(?MODULE, {update, Collection, Selector, Doc, Opts}).

%% ============================================================================
delete(Collection, Selector) ->
  gen_server:call(?MODULE, {delete, Collection, Selector, ?TIMEOUT}).

delete(Collection, Selector, Timeout) ->
  gen_server:call(?MODULE, {delete, Collection, Selector, Timeout}).

%% ============================================================================
% lela_repo:ensure_index(DB, #{
%   <<"key">> => #{<<"email">> => 1},
%   <<"unique">> => true}
% }).
ensure_index(Collection, IndexSpec) ->
  gen_server:call(?MODULE, {ensure_index, Collection, IndexSpec}).

%% ============================================================================
start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{seed, [Seed]}, {option, Option}, {woption, WOption}] = Args) ->
  ?DEBUG("Args = ~p", [Args]),
  {ok, Topology} = mongoc:connect(Seed, Option, WOption),
  {ok, #state{topo = Topology}}.

handle_call({topology}, _From, #state{topo = Topology} = State) ->
  {reply, Topology, State};

handle_call({find_one, Collection, Selector, Projector}, _From, #state{topo = Topology} = State) ->
  Fun = fun (Worker) ->
          mongoc:find_one(Worker, Collection, Selector, Projector, 0)
        end,
  Res = mongoc:transaction_query(Topology, Fun),
  {reply, {ok, Res}, State};

handle_call({find, Collection, Selector, Projector, Timeout}, _From, #state{topo = Topology} = State) ->
  Fun = fun(Worker) ->
          mongoc:find(Worker, Collection, Selector, Projector, 0, 0)
        end,
  Cursor = mongoc:transaction_query(Topology, Fun, [], Timeout),
  Data = mc_cursor:rest(Cursor),
  mc_cursor:close(Cursor),
  {reply, {ok, Data}, State};

handle_call({save, Collection, Doc, Timeout}, _From, #state{topo = Topology} = State) ->
  Res = mongo_api:insert(Topology, Collection, Doc, Timeout),
  {reply, {ok, Res}, State};

handle_call({update, Collection, Selector, Doc, Opts}, _From, #state{topo = Topology} = State) ->
  Res = mongo_api:update(Topology, Collection, Selector, Doc, Opts),
  {reply, {ok, Res}, State};

handle_call({delete, Collection, Selector, Timeout}, _From, #state{topo = Topology} = State) ->
  Res = mongo_api:delete(Topology, Collection, Selector, Timeout),
  {reply, {ok, Res}, State};

handle_call({ensure_index, Collection, IndexSpec}, _From, #state{topo = Topology} = State) ->
  Fun = fun(Worker) ->
          mc_worker_api:ensure_index(Worker, Collection, IndexSpec)
        end,
  Res = mongoc:transaction(Topology, Fun),
  {reply, {ok, Res}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

