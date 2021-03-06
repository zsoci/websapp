%%% HTTP COMMANDS
%%-define(GET, <<"GET">>).
%%-define(HEAD, <<"HEAD">>).
%%-define(OPTIONS, <<"OPTIONS">>).
%%-define(POST, <<"POST">>).
%%-define(PUT, <<"PUT">>).
%%-define(PATCH, <<"PATCH">>).
%%-define(DELETE, <<"DELETE">>).
%%
%%%%% HTTP HEADER FIELD elements
%%-define(APPLICATION, <<"application">>).
%%-define(JSON, <<"json">>).
%%-define(TEXT, <<"text">>).
%%-define(HTML, <<"html">>).
%%-define(PLAIN, <<"plain">>).
%%-define(CONTENT_TYPE, <<"content-type">>).
%%-define(APPLICATION_JSON, <<"application/json">>).
%%

-define(SERVICE_NAME, wsa_service).
-define(SERVICE_MODULE, wsa_service).
-define(WSA_SERVER_REF, wsa_server).


-type options() :: #{path => string(),
                     model => module(),
                     verbose => boolean()
                    }.

-type state() :: #{ opts => options()
                  }.

-export_type([state/0, options/0]).
