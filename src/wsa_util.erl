%%%-------------------------------------------------------------------
%%% @author zsoci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Dec 2016 5:00 PM
%%%-------------------------------------------------------------------
-module(wsa_util).
-author("zsoci").

%% API
-export([method_to_atom/1]).

-spec method_to_atom(binary() | string()) -> atom().
method_to_atom(<<"GET">>) -> get;
method_to_atom(<<"PATCH">>) -> patch;
method_to_atom(<<"PUT">>) -> put;
method_to_atom(<<"POST">>) -> post;
method_to_atom(<<"DELETE">>) -> delete;
method_to_atom("GET") -> get;
method_to_atom("PATCH") -> patch;
method_to_atom("PUT") -> put;
method_to_atom("POST") -> post;
method_to_atom("DELETE") -> delete.