-module(cfa).

-define(SERVICE_NAME, cfa_service).
-define(SERVICE_MODULE, cfa_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([get_handlers/0,
         add_routes/1,
         add_routes/2]).


start() -> csi:start(?SERVICE_NAME, ?SERVICE_MODULE).
start_link() -> csi:start_link(?SERVICE_NAME, ?SERVICE_MODULE).

stop() -> csi:stop(?SERVICE_NAME).

get_handlers() -> csi:call_s(?SERVICE_NAME, get_handlers, all).

add_routes(Routes) ->
  case application:get_application() of
    undefined ->
      {error, app_not_defined};
    {ok, App} ->
      add_routes(App, Routes)
  end.

add_routes(App, Routes) ->
  csi:call(?SERVICE_NAME, add_routes, {App, Routes}).