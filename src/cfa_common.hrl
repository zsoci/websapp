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

-type options() :: #{path => string(),
                     model => module(),
                     verbose => boolean()
                    }.

-type state() :: #{ opts => options()
                  }.

-export_type([state/0, options/0]).
