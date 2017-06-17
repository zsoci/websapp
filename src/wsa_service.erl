-module(wsa_service).
-behaviour(csi_server).

-include_lib("common_test/include/ct.hrl").
-include("wsa_common.hrl").

%% General state of the service
-record(wsa_server_state, { trail_handlers :: list(atom()),
                            pure_handlers :: dict:dict(),
                            middlewares :: list(atom()),
                            env :: proplists:proplist(),
                            trans_opts :: proplists:proplist(),
                            nr_of_acceptors :: pos_integer()
}).

%%-type wsa_state() :: dict:dict().
%%
%%%% Lifecycle State for every requests'
%%-type wsa_session_state() :: dict:dict().

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2,
         handle_call/3]).

-export([add_trail_handlers/2,
         trails/0,
         get_handlers/2,
         update_routes/2]).

-define(DEFAULT_NR_OF_ACCEPTORS, 5).
%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
  TransOpts = application:get_env(wsa, trans_opts, [{port, 8082}]),
  Acceptors = application:get_env(wsa, acceptors, ?DEFAULT_NR_OF_ACCEPTORS ),
  TrailHandlers = dict:store(wsa,
                             [ cowboy_swagger_handler
                             , wsa_healthcheck_handler
                             ],
                             dict:new()),
  State = #wsa_server_state{trail_handlers  = TrailHandlers,
                            pure_handlers   = dict:new(),
                            middlewares     = [ cowboy_router,
                                                cowboy_handler
                                              ],
                            env             = [{compress, true}],
                            trans_opts      = TransOpts,
                            nr_of_acceptors = Acceptors
  },
  start_server(dict:new(), ?WSA_SERVER_REF, State).

init(_Args, ServiceState) ->
  {ok, ServiceState}.

terminate(_Reason, _State) ->
  ok.

terminate_service(_Reason, _State) ->
  ok.

%% ====================================================================
%% Service functions
%% ====================================================================
get_handlers(all, State) ->
  {dict:to_list(State), State};
get_handlers(Server, State) ->
  {dict:find(Server, State), State}.

add_trail_handlers({Server, App, TrailHandlers}, ServiceState) ->
  State = dict:fetch(Server, ServiceState),
  #wsa_server_state{trail_handlers = TrailHandlersDict,
                    pure_handlers  = PureHandlersDict,
                    middlewares    = Middlewares,
                    env            = Env} = State,
  OldTrailModules = case dict:find(App, TrailHandlersDict) of
                      error ->
                        [];
                      {ok, Value} ->
                        Value
                    end,
  NewTModules = sets:to_list(sets:from_list(TrailHandlers ++ OldTrailModules)),
  NewTDict = dict:store(App, NewTModules, TrailHandlersDict),
  Values = set_routes(NewTDict, PureHandlersDict, Middlewares, Env),
  ranch:set_protocol_options(Server, Values),
  {ok, dict:store(Server, State#wsa_server_state{ trail_handlers = NewTDict},
                  ServiceState)}.

update_routes({Server, Routes}, ServiceState) ->
  State = dict:fetch(Server, ServiceState),
  #wsa_server_state{trail_handlers = TrailHandlers,
                    pure_handlers  = PureHandlers,
                    middlewares    = Middlewares,
                    env            = Env} = State,
  NewPureHandlers = case Routes of
                      [] ->
                        PureHandlers;
                      UpdRoutes ->
                        dict:store(Server, UpdRoutes, PureHandlers)
                    end,
  Values = set_routes(TrailHandlers, NewPureHandlers, Middlewares, Env),
  ranch:set_protocol_options(Server, Values),
  {ok, dict:store(Server,
                  State#wsa_server_state{ pure_handlers = NewPureHandlers})}.

start_server(ServiceState, ServerName,
             State = #wsa_server_state{trail_handlers  = TrailHandlers,
                                       pure_handlers   = PureHandlers,
                                       middlewares     = Middlewares,
                                       env             = Env,
                                       trans_opts      = TransOpts,
                                       nr_of_acceptors = Acceptors}) ->
  ProtoOpts = set_routes(TrailHandlers, PureHandlers, Middlewares, Env),
  case cowboy:start_http(ServerName, Acceptors, TransOpts, ProtoOpts) of
    {ok, _} ->
      {ok, dict:store(ServerName, State, ServiceState)};
    {error, {already_started, _}} ->
      {already_started, ServiceState}
  end.

set_routes(TrailDict, PureDict, Middlewares, Env) ->
  _ = put(dummy_handlers, PureDict),
  TrailHandlers = dict:fold( fun(_, Value, AccIn) ->
                               Value ++ AccIn
                             end, [], TrailDict),
  TrailRoutes = trails:trails([?MODULE | TrailHandlers]),
  trails:store(TrailRoutes),
  Dispatch = trails:single_host_compile(TrailRoutes),
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
       parameters => [RequestBody],
       produces => ["text/html"],
       consumes => ["text/html"]
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
