-module(cfa_service).
-behaviour(csi_server).

%% General state of the service
-record(cfa_state, {trail_handlers :: list(atom()),
                    pure_handlers :: dict:dict(),
                    middlewares :: list(atom()),
                    env :: proplists:proplist(),
                    trans_opts :: proplists:proplist(),
                    nr_of_acceptors :: pos_integer()
                   }).

%% Lifecycle State for every requests'
-record(cfa_session_state, {}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

-export([trails/0,
         process_too_long/2,
         process_crashing/2]).

-define(DEFAULT_NR_OF_ACCEPTORS, 5).
%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
  TransOpts = application:get_env(cfa, trans_opts, [{port, 8082}]),
  Acceptors = application:get_env(cfa, acceptors, ?DEFAULT_NR_OF_ACCEPTORS ),
  State = #cfa_state{trail_handlers = [ cowboy_swagger_handler ],
                     pure_handlers = dict:new(),
                     middlewares = [
                       cowboy_router,
                       cowboy_handler
                     ],
                     env = [{compress, true}],
                     trans_opts = TransOpts,
                     nr_of_acceptors = Acceptors
    },
  undefined = put(dummy_handlers, State#cfa_state.pure_handlers),
  {ok, update_routes(State)}.

init(_Args, _ServiceState) ->
  {ok, #cfa_session_state{}}.

terminate(_Reason, _State) ->
  ok.

terminate_service(_Reason, _State) ->
  ok.

%% ====================================================================
%% Service functions
%% ====================================================================
process_foo(_Args, State) ->
  {hello_world, State}.

process_too_long(_Args, State) ->
  {ok, Sleep} = application:get_env(cfa, timer_sleep),
  timer:sleep(Sleep),
  {long_job_finished, State}.

process_crashing(Args, State) ->
  A = Args - Args,
  {A, State}.

update_routes(State = #cfa_state{trail_handlers = TrailHandlers,
                                 middlewares = Middlewares,
                                 env = Env,
                                 trans_opts = TransOpts,
                                 nr_of_acceptors = Acceptors
  }) ->
  TrailRoutes = trails:trails([?MODULE | TrailHandlers]),
  Dispatch = trails:single_host_compile(TrailRoutes),
  ProtoOpts = [{env, [{dispatch, Dispatch} | Env]},
               {middlewares, Middlewares}],
  case cowboy:start_http(cfa_server, Acceptors, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end,
  State.

-spec trails() -> trails:trails().
trails() ->
  [ make_trails(Dummy) || Dummy <- dict:to_list(get(dummy_handlers))].

make_trails({Name, {Host, PathDefs}}) ->
  make_trails({Name, {Host, [], PathDefs}});
make_trails({Name, {_Host, _Constraints, PathDefs}}) ->
  [ make_a_trail(Name, Path) || Path <- PathDefs].

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
    #{ tags => [Name],
       description => Name ++ " is missing swagger definition",
       parameters => [RequestBody]
    },
  Metadata =
    #{ post => MethodData,
       get => MethodData
    },
  Opts = #{ path => Path,
            model => zsocimodel,
            verbose => false,
            function => login
         },
  trails:trail(Path, Module, Opts, Metadata).

%%Dispatch = cowboy_router:compile([
%%{'_', [
%%{"/", ctr_healthcheck_handler, []},
%%{"/favicon.ico", cowboy_static, {priv_file, ctr, "favicon.ico"}},
%%{"/ping", ctr_healthcheck_handler, []},
%%{"/counter/inc", ctr_counter_handler, []},
%%{"/counter/dec", ctr_counter_handler, []},
%%{"/counter/create", ctr_counter_handler, []},
%%{"/counter/ix", ctr_counter_handler, []},
%%{"/counter/[:id]", ctr_counter_handler, []},
%%
%%{'_', ctr_rest_handler, []}]}
%%]),
%%cowboy:start_http(ctr_rest_listener, 5, [{port, CowboyPort}],
%%[{env, [{dispatch, Dispatch}]},
%%{middlewares, [cowboy_router,
%%cowboy_handler]}]),
%%
%%
%%{'_',[],[{[],[],fen_healthcheck_handler,#{model => zsocimodel,path => "/",verbose => true}},{[<<"v1">>,<<"auth">>,<<"priv">>,<<"login">>],[],fen_auth_handler,#{function => login,model => zsocimodel,path => "/v1/auth/priv/login",verbose => false}},{[<<"v1">>,<<"auth">>,<<"priv">>,<<"register">>],[],fen_auth_handler,#{function => register,model => zsocimodel,path => "/v1/auth/priv/register",verbose => false}},{[<<"v1">>,<<"message">>,<<"get">>],[],fen_get_handler,#{function => get,model => zsocimodel,path => "/v1/message/get",verbose => false}},{[<<"v1">>,resource,function],[],fen_set_handler,#{function => set,model => zsocimodel,path => "/v1/:resource/:function",verbose => false}},{[<<"api-docs">>],[],cowboy_swagger_redirect_handler,{file,"/Users/zsoci/Projects/hive/FrontEnd/_build/default/rel/fen/lib/cowboy_swagger-1.1.0/priv/swagger/index.html"}},{[<<"api-docs">>,<<"swagger.json">>],[],cowboy_swagger_json_handler,#{}},{[<<"api-docs">>,'...'],[],cowboy_static,{dir,"/Users/zsoci/Projects/hive/FrontEnd/_build/default/rel/fen/lib/cowboy_swagger-1.1.0/priv/swagger",[{mimetypes,cow_mimetypes,all}]}}]}