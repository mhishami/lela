-module (lela_users_update_handler).
-include ("lela.hrl").

%% Standard callbacks.
-export ([init/3]).
-export ([allowed_methods/2]).
-export ([content_types_provided/2]).
-export ([content_types_accepted/2]).
-export ([delete_resource/2, delete_completed/2]).
-export ([resource_exists/2]).

%% API Implementation
-export ([update_user/2]).
-export ([get_user/2]).

-import (lela_users_handler, [ensure_exists/2]).

%%% ===========================================================================
%%% cowboy callbacks
%%% ===========================================================================
init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>], Req, State}.

%  for GET
content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_user}], Req, State}.

% for PUT, PATCH
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, update_user}], Req, State}.

% for DELETE
delete_resource(Req, State) ->
  {Id, Req1} = cowboy_req:binding(id, Req),
  Res = lela_user:delete(#{lela_user:id() => Id}),
  {false, Req1, State}.

delete_completed(Req, State) ->
  {true, Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

update_user(Req, State) ->
  try
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Id, Req2} = cowboy_req:binding(id, Req1),
    Data = jsx:decode(Body, [return_maps]),

    ensure_exists([lela_user:password()], Data),
    Password = maps:get(lela_user:password(), Data),

    {ok, 1, 1, User} = lela_user:change_password(Id, Password),

    % remove password from sending
    Res = maps:remove(lela_user:password(), User),
    Req3 = cowboy_req:set_resp_body(jsx:encode(Res), Req2),
    {true, Req3, State}
  catch
    _:Error ->
      ?ERROR("Error: ~p", [Error]),
      Req4 = cowboy_req:set_resp_body(
          jsx:encode([{error, <<"Malformed JSON request, or unknown data">>}]), Req),
      {false, Req4, State}
  end.

get_user(Req, State) ->
  try
    {Id, Req1} = cowboy_req:binding(id, Req),
    {ok, [User]} = lela_user:find(#{lela_user:id() => Id}),

    % remove password from sending
    Res = maps:remove(lela_user:password(), User),
    {jsx:encode(Res), Req1, State}
  catch
    _:Error ->
      ?ERROR("Error: ~p", [Error]),
      Req4 = cowboy_req:set_resp_body(
          jsx:encode([{error, <<"Malformed JSON request">>}]), Req),
      {false, Req4, State}
  end.
