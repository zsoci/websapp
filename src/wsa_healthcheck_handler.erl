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
  {MediaType, Req3} = cowboy_req:header(<<"accept">>, Req2),
  Verbose = case Value of
              <<"true">> ->
                true;
              <<"false">> ->
                false;
              _ ->
                Value
            end,
  handle_get_ping_with_media_type(MediaType, Verbose, Req3, State).

handle_get_ping_with_media_type(<<"text/plain">>, Verbose, Req, State) ->
  Reply = case Verbose of
            true ->
              <<"Verbose Pong">>;
            _ ->
              <<"Pong">>
          end,
  {Reply, Req, State};
handle_get_ping_with_media_type(<<"application/json">>, Verbose, Req, State) ->
  Reply = case Verbose of
            true ->
              [{<<"response">>, <<"Verbose Pong">>}];
            _ ->
              [{<<"response">>, <<"Pong">>}]
          end,
  {jsx:encode(Reply), Req, State}.

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
        produces => ["text/plain", "application/json"],
        parameters => [Parameter],
        handler => handle_get_ping,
        responses =>
        #{ <<"200">> =>
           #{ description => "Document returned",
              schema =>
              #{ type => object,
                 properties =>
                 #{ response =>
                    #{ type => string,
                       description => "Pong message"
                    }
                 }
              }
           }
        }

     }
  },
  Path = "/ping",
  Opts = #{ path => Path,
            server => ?WSA_SERVER_REF,
            verbose => true
  },
  trails:trail(Path, ?MODULE, Opts, Metadata).

trails_root() ->
  Metadata =
    #{ get =>
       #{ tags => ["Health Check"],
          description => "Returns an empty body for load balancer",
          produces => ["text/plain"],
          handler => handle_get_root,
          responses =>
          #{ <<"200">> =>
             #{ description => "OK"
             }
          }
       }
    },
  Path = "/",
  Opts = #{ path => Path,
            server => ?WSA_SERVER_REF,
            verbose => true
         },
  trails:trail(Path, ?MODULE, Opts, Metadata).