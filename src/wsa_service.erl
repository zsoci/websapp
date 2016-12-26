-module(wsa_service).
-behaviour(csi_server).

-include_lib("common_test/include/ct.hrl").

%% General state of the service
-record(wsa_state, {trail_handlers :: list(atom()),
                    pure_handlers :: dict:dict(),
                    middlewares :: list(atom()),
                    env :: proplists:proplist(),
                    trans_opts :: proplists:proplist(),
                    nr_of_acceptors :: pos_integer()
}).

%% Lifecycle State for every requests'
-record(wsa_session_state, {wsa_state :: #wsa_state{}}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2,
         handle_call/3]).

-export([trails/0,
         get_handlers/2,
         update_routes/2]).

-define(DEFAULT_NR_OF_ACCEPTORS, 5).
-define(WSA_SERVER_REF, wsa_server).
%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
  TransOpts = application:get_env(wsa, trans_opts, [{port, 8082}]),
  Acceptors = application:get_env(wsa, acceptors, ?DEFAULT_NR_OF_ACCEPTORS ),
  TrailHandlers = dict:store(wsa, [ cowboy_swagger_handler,
                                    wsa_healthcheck_handler ], dict:new()),
  State = #wsa_state{trail_handlers  = TrailHandlers,
                     pure_handlers   = dict:new(),
                     middlewares     = [
                       cowboy_router,
                       cowboy_handler
                     ],
                     env             = [{compress, true}],
                     trans_opts      = TransOpts,
                     nr_of_acceptors = Acceptors
  },
  {ok, start_server(State)}.

init(_Args, ServiceState) ->
  {ok, #wsa_session_state{wsa_state = ServiceState}}.

terminate(_Reason, _State) ->
  ok.

terminate_service(_Reason, _State) ->
  ok.

%% ====================================================================
%% Service functions
%% ====================================================================
get_handlers(all, State = #wsa_session_state{wsa_state = WsaState}) ->
  {[{trail_handlers, dict:to_list(WsaState#wsa_state.trail_handlers)},
    {cowboy_handlers, dict:to_list(WsaState#wsa_state.pure_handlers)}],
   State}.

update_routes({App, Routes}, State = #wsa_state{trail_handlers = TrailHandlers,
                                                pure_handlers  = PureHandlers,
                                                middlewares    = Middlewares,
                                                env            = Env}) ->
  NewPureHandlers = dict:store(App, Routes, PureHandlers),
  Values = set_routes(TrailHandlers, NewPureHandlers, Middlewares, Env),
  {lists:foreach(fun({Key, Value}) ->
                   cowboy:set_env(?WSA_SERVER_REF, Key, Value)
                 end,
                 Values),
   State}.

start_server(State = #wsa_state{trail_handlers  = TrailHandlers,
                                pure_handlers   = PureHandlers,
                                middlewares     = Middlewares,
                                env             = Env,
                                trans_opts      = TransOpts,
                                nr_of_acceptors = Acceptors}) ->
  ProtoOpts = set_routes(TrailHandlers, PureHandlers, Middlewares, Env),
  case cowboy:start_http(?WSA_SERVER_REF, Acceptors, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end,
  State.

set_routes(TrailDict, PureDict, Middlewares, Env) ->
  _ = put(dummy_handlers, PureDict),
  TrailHandlers = dict:fold( fun(_, Value, AccIn) ->
                               Value ++ AccIn
                             end, [], TrailDict),
  TrailRoutes = trails:trails([?MODULE | TrailHandlers]),
  trails:store(TrailRoutes),
  Dispatch = trails:single_host_compile(TrailRoutes),
%%  [{'_',[],
%%    [{[<<"ping">>],
%%      [],ctr_healthcheck_handler,
%%      #{function => login,
%%        model => zsocimodel,
%%        path => "/ping",
%%        verbose => false}},
%%     {[<<"api-docs">>],
%%      [],cowboy_swagger_redirect_handler,
%%      {file,
%%       "/Users/zsoci/Projects/hive/PubSubHub/newrestctr/_build/default/lib/cowboy_swagger/priv/swagger/index.html"}},
%%     {[<<"api-docs">>,<<"swagger.json">>],
%%      [],cowboy_swagger_json_handler,#{}},
%%     {[<<"api-docs">>,'...'],
%%      [],cowboy_static,
%%      {dir,
%%       "/Users/zsoci/Projects/hive/PubSubHub/newrestctr/_build/default/lib/cowboy_swagger/priv/swagger",
%%       [{mimetypes,cow_mimetypes,all}]}},
%%     {[],[],cfa_healthcheck_handler,
%%      #{path => "/",verbose => true}}]}],
%%
%%
%%  [{'_',[],
%%    [{[<<"favicon.ico">>],
%%      [],cowboy_static,
%%      {priv_file,ctr,"favicon.ico"}},
%%     {'_',[],ctr_rest_handler,[]}]}],
%%  D2 =cowboy_router:compile([
%%                                              {'_', [
%%                                                {"/favicon.ico", cowboy_static, {priv_file, ctr, "favicon.ico"}},
%%
%%
%%                                                {'_', ctr_rest_handler, []}]}]),
%%  ct:pal("(zsoci) ~p(~p): {Dispatch}:~p",
%%         [?MODULE, ?LINE, {D2}]),
  [{env, [{dispatch, Dispatch} | Env]}, {middlewares, Middlewares}].
  
-spec trails() -> trails:trails().
trails() ->
  lists:foldl(fun(Dummy, AccIn) ->
                make_trails(Dummy) ++ AccIn
              end, [], dict:to_list(get(dummy_handlers))).

make_trails({Name, HostList}) when is_list(HostList) ->
  lists:foldl(fun(HostRoute, AccIn) ->
                make_trails({Name, HostRoute}) ++ AccIn
              end, [], HostList);
make_trails({Name, {Host, PathDefs}}) ->
  make_trails({Name, {Host, [], PathDefs}});
make_trails({Name, {_Host, _Constraints, PathDefs}}) ->
  [ make_a_trail(atom_to_list(Name), Path) || Path <- PathDefs,
                                              path_not_wildcard(Path)].

path_not_wildcard({Path, _, _}) ->
  '_' =/= Path.

make_a_trail(Name, {Path, Module, _Options}) ->
  RequestBody =
    #{ name => <<"request body">>,
       in => body,
       description => <<"request body">>,
       required => false,
       schema =>
       #{ type => string
       }
    },
  MethodData =
    #{ tags => [Name ++ " (application)"],
       description => Name ++ " is missing swagger definition",
       parameters => [RequestBody]
    },
  Metadata =
    #{
       get => MethodData
    },
  Opts = #{ path => Path,
            model => zsocimodel,
            verbose => false,
            function => login
         },
  trails:trail(Path, Module, Opts, Metadata).

handle_call({Request, Args}, _From, State) ->
  lager:info("Calling service through handle_call(~p)",
             [{Request, Args}]),
  {Reply, NewState} = ?MODULE:Request(Args, State),
  {reply, Reply, NewState}.
