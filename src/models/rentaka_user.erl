-module (rentaka_user).

-include ("rentaka.hrl").

-export ([new/2]).
-export ([get_email/1, get_password/1]).

-export ([find_by_email/1, update_password/2, authenticate/2]).
-export ([save/1, update/1, delete/1, find/1, find/2]).

-define (DB, <<"users">>).

-define (ID, <<"_id">>).
-define (EMAIL, <<"email">>).
-define (PASS, <<"password">>).
-define (CREATED, <<"created_at">>).
-define (UPDATED, <<"updated_at">>).

new(Email, Password) when is_binary(Email),
                          is_binary(Password) ->
  Now = iso8601:format(calendar:local_time()),
  Uuid = uuid:uuid_to_string(uuid:get_v4()),
  #{
    ?ID      => list_to_binary(Uuid),
    ?EMAIL   => Email,
    ?PASS    => erlpass:hash(Password),
    ?CREATED => Now,
    ?UPDATED => Now
  }.

%%% ===========================================================================
%%% Helper functions
%%% ===========================================================================
get_email(User) -> maps:get(?EMAIL, User).

get_password(User) -> maps:get(?PASS, User).

%%% ===========================================================================
%%% API functions
%%% ===========================================================================
find_by_email(Email) when is_binary(Email) ->
  case rentaka_repo:find_one(?DB, #{?EMAIL => Email}) of
    {ok, Rec} -> Rec;
    Else      -> {error, Else}
  end.

update_password(User, Password) when is_map(User), is_binary(Password) ->
  Updated = User#{?PASS => erlpass:hash(Password)},
  update(Updated).

authenticate(Email, Password) when is_binary(Email), is_binary(Password) ->
  User = find_by_email(Email),
  erlpass:match(Password, maps:get(?PASS, User)).

%%% ===========================================================================
%%% Generic Methods
%%% ===========================================================================
save(User) when is_map(User) ->
  {ok, {{true, #{<<"n">> := N}}, U}} = rentaka_repo:save(?DB, User),
  {ok, N, U}.

update(User) when is_map(User) ->
  Selector = #{?EMAIL => maps:get(?EMAIL, User)},
  Updated = User#{?UPDATED => iso8601:format(calendar:local_time())},
  {ok,{true,#{<<"n">> := N, <<"nModified">> := M}}} = rentaka_repo:update(?DB, Selector, Updated),
  {ok, N, M, Updated}.

delete(Selector) when is_map(Selector) ->
  {ok, {true, #{<<"n">> := N}}} = rentaka_repo:delete(?DB, Selector),
  {ok, N}.

find(Selector) when is_map(Selector) ->
  rentaka_repo:find(?DB, Selector).

find(Selector, Projector) when is_map(Selector), is_map(Projector) ->
  rentaka_repo:find(?DB, Selector, Projector).



