%%%-------------------------------------------------------------------
%%% @author zsoci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Dec 2016 7:05 PM
%%%-------------------------------------------------------------------
-module(wsa_default_handler).
-author("zsoci").

-include("wsa_common.hrl").

-export([ init/3,
          rest_init/2,
          allowed_methods/2,
          resource_exists/2,
          content_types_accepted/2,
          content_types_provided/2
        ]).

-export([ announce_req/2
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cowboy Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Upgrades to cowboy_rest.
%%      Basically, just returns <code>{upgrade, protocol, cowboy_rest}</code>
%% @see cowboy_rest:init/3
-spec init({atom(), atom()}, cowboy_req:req(), options()) ->
  {upgrade, protocol, cowboy_rest}.
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%% @doc Announces the Req and moves on.
%%      If <code>verbose := true</code> in <code>Opts</code> for this handler
%%      prints out a line indicating that endpoint that was hit.
%% @see cowboy_rest:rest_init/2
-spec rest_init(cowboy_req:req(), options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Opts) ->
  Req1 = announce_req(Req, Opts),
  {ok, Req1, #{opts => Opts}}.

%% @doc Retrieves the list of allowed methods from Trails metadata.
%%      Parses the metadata associated with this path and returns the
%%      corresponding list of endpoints.
%% @see cowboy_rest:allowed_methods/2
-spec allowed_methods(cowboy_req:req(), state()) ->
  {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
  #{opts := #{path := Path}} = State,
  #{metadata := Metadata} = trails:retrieve(Path),
  Methods = [atom_to_method(Method) || Method <- maps:keys(Metadata)],
  {Methods, Req, State}.

%% @doc Returns <code>false</code> for POST, <code>true</code> otherwise.
%% @see cowboy_rest:resource_exists/2
-spec resource_exists(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Method =/= <<"POST">>, Req1, State}.

%% @doc Always returns "application/json *" with <code>handle_post</code>.
%% @see cowboy_rest:content_types_accepted/2
%% @todo Use swagger's 'consumes' to auto-generate this if possible
%%       <a href="https://github.com/inaka/sumo_rest/issues/7">Issue</a>
-spec content_types_accepted(cowboy_req:req(), state()) ->
  {[{{binary(), binary(), '*'}, atom()}], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
  #{opts := #{path := Path}} = State,
  {Method, Req2} = cowboy_req:method(Req),
  try
    #{metadata := Metadata} = trails:retrieve(Path),
    AtomMethod = method_to_atom(Method),
    #{AtomMethod := #{consumes := Consumes}} = Metadata,
    Handler = compose_handler_name(AtomMethod),
    RetList = [{iolist_to_binary(X), Handler} || X <- Consumes],
    {RetList, Req2, State}
  catch
    _:_ ->
      {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}
  end.

%% @doc Always returns "application/json" with <code>handle_get</code>.
%% @see cowboy_rest:content_types_provided/2
%% @todo Use swagger's 'produces' to auto-generate this if possible
%%       <a href="https://github.com/inaka/sumo_rest/issues/7">Issue</a>
-spec content_types_provided(cowboy_req:req(), state()) ->
  {[{binary(), atom()}], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
  #{opts := #{path := Path}} = State,
  {Method, Req2} = cowboy_req:method(Req),
  try
    #{metadata := Metadata} = trails:retrieve(Path),
    AtomMethod = method_to_atom(Method),
    #{AtomMethod := #{produces := Produces}} = Metadata,
    Handler = compose_handler_name(AtomMethod),
    RetList = [{iolist_to_binary(X), Handler} || X <- Produces],
    {RetList, Req2, State}
  catch
    _:_ ->
      {[{<<"application/json">>, handle_get}], Req, State}
  end.

%% @doc Announces the Req.
%%      If <code>verbose := true</code> in <code>Opts</code> for this handler
%%      prints out a line indicating that endpoint that was hit.
%% @see cowboy_rest:rest_init/2
-spec announce_req(cowboy_req:req(), options()) -> cowboy_req:req().
announce_req(Req, #{verbose := true}) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  _ = error_logger:info_msg("~s ~s", [Method, Path]),
  Req2;
announce_req(Req, _Opts) -> Req.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Auxiliary Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec atom_to_method(get|patch|put|post|delete) -> binary().
atom_to_method(get) -> <<"GET">>;
atom_to_method(patch) -> <<"PATCH">>;
atom_to_method(put) -> <<"PUT">>;
atom_to_method(post) -> <<"POST">>;
atom_to_method(delete) -> <<"DELETE">>.

-spec method_to_atom(binary()) -> atom().
method_to_atom(<<"GET">>) -> get;
method_to_atom(<<"PATCH">>) -> patch;
method_to_atom(<<"PUT">>) -> put;
method_to_atom(<<"POST">>) -> post;
method_to_atom(<<"DELETE">>) -> delete.

-spec compose_handler_name(get|patch|put|post) -> atom().
compose_handler_name(get) -> handle_get;
compose_handler_name(put) -> handle_put;
compose_handler_name(patch) -> handle_patch;
compose_handler_name(post) -> handle_post.