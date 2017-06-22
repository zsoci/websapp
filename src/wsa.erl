-module(wsa).

-include("wsa_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([add_trail_handlers/1,
         add_trail_handlers/2,
         add_trail_handlers/3,
         get_handlers/0,
         get_handlers/1,
         start_server/5,
         stop_server/1,
         update_routes/1,
         update_routes/2]).


start() -> csi:start(?SERVICE_NAME, ?SERVICE_MODULE).
start_link() -> csi:start_link(?SERVICE_NAME, ?SERVICE_MODULE).

stop() -> csi:stop(?SERVICE_NAME).

add_trail_handlers(TrailHandlers) ->
  case application:get_application() of
    undefined ->
      {error, app_not_defined};
    {ok, App} ->
      add_trail_handlers(TrailHandlers,App)
    end.

add_trail_handlers(TrailHandlers, App) ->
  add_trail_handlers(TrailHandlers, App, ?WSA_SERVER_REF).

add_trail_handlers(TrailHandlers, App, Server) ->
  csi:call(?SERVICE_NAME, add_trail_handlers, {Server, App, TrailHandlers}).

get_handlers() ->
  get_handlers(all).

get_handlers(Server) ->
  csi:call_s(?SERVICE_NAME, get_handlers, Server).

-spec start_server(Server :: atom(),
                   TransOpt :: proplists:proplist(),
                   Acceptors :: pos_integer(),
                   TrailHandlers :: list(atom()),
                   App :: atom()) ->
  ok | {error, Reason :: any()}.
start_server(Server, TransOpt, Acceptors, TrailHandlers, App) ->
  csi:call(?SERVICE_NAME, start_new_server,
           {Server, TransOpt, Acceptors, TrailHandlers, App}).

stop_server(Server) ->
  csi:call(?SERVICE_NAME, stop_server, Server).

update_routes(Routes) ->
  case application:get_application() of
    undefined ->
      {error, app_not_defined};
    {ok, App} ->
      update_routes(App, Routes)
  end.

update_routes(App, Routes) ->
  csi:call(?SERVICE_NAME, update_routes, {App, Routes}).