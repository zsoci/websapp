%%%-------------------------------------------------------------------
%%% @author zsoci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Dec 2016 7:03 PM
%%%-------------------------------------------------------------------
-module(wsa_healthcheck_handler).
-author("zsoci").
-behaviour(trails_handler).

-include("wsa_common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("mixer/include/mixer.hrl").

-mixin([{ wsa_default_handler,
          [ init/3,
            rest_init/2,
            allowed_methods/2,
            resource_exists/2,
            content_types_accepted/2,
            content_types_provided/2
          ]
        }]).

-export([handle_get_root/2,
         handle_get_ping/2
        ]).

-export([ trails/0
        ]).

-spec handle_get_ping(Req :: cowboy_req:req(),
                          State :: fen_common:state()) ->
                           Result :: {iodata(), cowboy_req:req(), state()}.
handle_get_ping(Req, State) ->
  {Value, Req2} = cowboy_req:qs_val(<<"verbose">>, Req, false),
  Verbose = case Value of
              <<"true">> ->
                true;
              <<"false">> ->
                false;
              _ ->
                Value
            end,
  Reply = case Verbose of
            true ->
              <<"verbose pong">>;
            _ ->
              <<"pong">>
          end,
  {Reply, Req2, State}.

-spec handle_get_root(Req :: cowboy_req:req(),
                          State :: fen_common:state()) ->
                           Result :: {iodata(), cowboy_req:req(), state()}.
handle_get_root(Req, State) ->
  {<<"">>, Req, State}.

-spec trails() -> trails:trails().
trails() ->
  [ trails_root()
  , trails_ping()
  ].

trails_ping() ->
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
        parameters => [Parameter],
        handler => handle_get_ping
     }
  },
  Path = "/ping",
  Opts = #{ path => Path,
            verbose => true
  },
  trails:trail(Path, ?MODULE, Opts, Metadata).

trails_root() ->
  Metadata =
    #{ get =>
       #{ tags => ["Health Check"],
          description => "Returns an empty body for load balancer",
          produces => ["text/plain"],
          handler => handle_get_root
       }
    },
  Path = "/",
  Opts = #{ path => Path,
            verbose => true
         },
  trails:trail(Path, ?MODULE, Opts, Metadata).