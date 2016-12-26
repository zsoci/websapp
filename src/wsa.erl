-module(wsa).

-include("wsa_common.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([get_handlers/0,
         update_routes/1,
         update_routes/2]).


start() -> csi:start(?SERVICE_NAME, ?SERVICE_MODULE).
start_link() -> csi:start_link(?SERVICE_NAME, ?SERVICE_MODULE).

stop() -> csi:stop(?SERVICE_NAME).

get_handlers() -> csi:call_s(?SERVICE_NAME, get_handlers, all).

update_routes(Routes) ->
  case application:get_application() of
    undefined ->
      {error, app_not_defined};
    {ok, App} ->
      update_routes(App, Routes)
  end.

update_routes(App, Routes) ->
  csi:call(?SERVICE_NAME, update_routes, {App, Routes}).