-module(wsa_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Server = { wsa, { wsa, start_link, []},
                  permanent, 2000, worker, [wsa]},
    Procs = [Server],
    {ok, { {one_for_one, 3, 10}, Procs}}
.