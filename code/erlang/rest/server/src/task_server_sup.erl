-module(task_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 10,
        period => 10
    },
    
    ChildSpecs = [
        #{
            id => task_store,
            start => {task_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [task_store]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.