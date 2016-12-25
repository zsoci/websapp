-module(cfa_service).
-behaviour(csi_server).

-include_lib("common_test/include/ct.hrl").

%% General state of the service
-record(cfa_state, {trail_handlers :: list(atom()),
                    pure_handlers :: dict:dict(),
                    middlewares :: list(atom()),
                    env :: proplists:proplist(),
                    trans_opts :: proplists:proplist(),
                    nr_of_acceptors :: pos_integer()
                   }).

%% Lifecycle State for every requests'
-record(cfa_session_state, {cfa_state :: #cfa_state{}}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2,
         handle_call/3]).

-export([trails/0,
         get_handlers/2,
         add_routes/2]).

-define(DEFAULT_NR_OF_ACCEPTORS, 5).
-define(CFA_SERVER_REF, cfa_server).
%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
  TransOpts = application:get_env(cfa, trans_opts, [{port, 8082}]),
  Acceptors = application:get_env(cfa, acceptors, ?DEFAULT_NR_OF_ACCEPTORS ),
  TrailHandlers = dict:store(cfa, [ cowboy_swagger_handler,
                                    cfa_healthcheck_handler ], dict:new()),
  State = #cfa_state{trail_handlers = TrailHandlers,
                     pure_handlers = dict:new(),
                     middlewares = [
                       cowboy_router,
                       cowboy_handler
                     ],
                     env = [{compress, true}],
                     trans_opts = TransOpts,
                     nr_of_acceptors = Acceptors
    },
  {ok, start_server(State)}.

init(_Args, ServiceState) ->
  {ok, #cfa_session_state{cfa_state = ServiceState}}.

terminate(_Reason, _State) ->
  ok.

terminate_service(_Reason, _State) ->
  ok.

%% ====================================================================
%% Service functions
%% ====================================================================
get_handlers(all, State = #cfa_session_state{cfa_state = CfaState}) ->
  {[{trail_handlers, dict:to_list(CfaState#cfa_state.trail_handlers)},
    {cowboy_handlers, dict:to_list(CfaState#cfa_state.pure_handlers)}],
   State}.

add_routes({App, Routes}, State = #cfa_state{trail_handlers  = TrailHandlers,
                                             pure_handlers   = PureHandlers,
                                             middlewares     = Middlewares,
                                             env             = Env}) ->
  NewPureHandlers = dict:store(App, Routes, PureHandlers),
  Values = set_routes(TrailHandlers, NewPureHandlers, Middlewares, Env),
  {lists:foreach(fun({Key, Value}) ->
                   cowboy:set_env(?CFA_SERVER_REF, Key, Value)
                 end,
                 Values),
   State}.

start_server(State = #cfa_state{trail_handlers  = TrailHandlers,
                                pure_handlers   = PureHandlers,
                                middlewares     = Middlewares,
                                env             = Env,
                                trans_opts      = TransOpts,
                                nr_of_acceptors = Acceptors}) ->
  ProtoOpts = set_routes(TrailHandlers, PureHandlers, Middlewares, Env),
  case cowboy:start_http(?CFA_SERVER_REF, Acceptors, TransOpts, ProtoOpts) of
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
  ct:pal("(zsoci) ~p(~p): {TrailRoutes}:~p",
        [?MODULE, ?LINE, {TrailRoutes}]),
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
