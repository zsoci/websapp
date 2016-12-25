%%%-------------------------------------------------------------------
%%% @author zsoci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Dec 2016 7:03 PM
%%%-------------------------------------------------------------------
-module(cfa_healthcheck_handler).
-author("zsoci").
-behaviour(trails_handler).

-include("cfa_common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("mixer/include/mixer.hrl").

-mixin([{ cfa_default_handler,
          [ init/3,
            rest_init/2,
            allowed_methods/2,
            resource_exists/2,
            content_types_accepted/2,
            content_types_provided/2
          ]
        }]).

-export([handle_get/2
        ]).

-export([ trails/0
        ]).

-spec handle_get(Req :: cowboy_req:req(), State :: fen_common:state()) ->
  Result :: {iodata(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  lager:debug("Health", []),
  {Value, Req2} = cowboy_req:qs_val(<<"verbose">>, Req, false),
  Verbose = case Value of
              <<"true">> ->
                true;
              <<"false">> ->
                false;
              _ ->
                Value
            end,
  case Verbose of
    true ->
      {<<"Pong">>, Req2, State};
    _ ->
      {<<"">>, Req, State}
  end.

-spec trails() -> trails:trails().
trails() ->
  Parameter =
  #{ name => <<"verbose">>,
     in => query,
     type => boolean,
     allowEmptyValue => true,
     default => <<"false">>,
     description => <<"verbose parameter">>,
     required => false
  },
  Metadata =
  #{ get =>
     #{ tags => ["Health Check"],
        description => "Returns an empty body for load balancer",
        produces => ["text/plain"],
        parameters => [Parameter]
     }
  },
  Path = "/",
  Opts = #{ path => Path,
            verbose => true
  },
  [trails:trail(Path, ?MODULE, Opts, Metadata)].

