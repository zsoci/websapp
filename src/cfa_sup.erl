-module(cfa_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Server = { cfa, { cfa, start_link, []},
                  permanent, 2000, worker, [cfa]},
    Procs = [Server],
    {ok, { {one_for_one, 3, 10}, Procs}}
.