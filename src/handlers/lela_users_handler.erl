-module (lela_users_handler).

-include ("lela.hrl").

%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% API Implementation
-export ([list_users/2]).
-export ([create_user/2]).
-export ([ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, list_users}], Req, State}.

% for POST
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, create_user}], Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

list_users(Req, State) ->
  {ok, Users} = lela_user:find(#{}, #{
                  lela_user:email() => 1,
                  lela_user:id() => 1,
                  lela_user:created_at() => 1,
                  lela_user:updated_at() => 1
                }),
  % ?DEBUG("Users: ~p", [Users]),
  {jsx:encode(Users), Req, State}.

create_user(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    Data = jsx:decode(Body, [return_maps]),
    ensure_exists([lela_user:email(), lela_user:password()], Data),

    User = lela_user:new(
      maps:get(lela_user:email(), Data),
      maps:get(lela_user:password(), Data)
    ),
    {ok, 1, Res} = lela_user:save(User),
    Id = maps:get(lela_user:id(), Res),

    URI = <<"/users/", Id/binary >>,
    ?DEBUG("User URI: ~p", [URI]),
    {{true, URI}, Req1, State}
  catch
    _:Error ->
      ?ERROR("Error: ~p", [Error]),
      Msg = jsx:encode([{error, <<"Malformed JSON request, or data exists">>}]),
      Req3 = cowboy_req:set_resp_body(Msg, Req),
      {false, Req3, State}
  end.

%% ============================================================================
%% Private functions
%%
ensure_exists([H|T], Data) ->
  % ?DEBUG("Checking for field ~p", [H]),
  maps:get(H, Data),
  ensure_exists(T, Data);

ensure_exists([], _Data) -> ok.

