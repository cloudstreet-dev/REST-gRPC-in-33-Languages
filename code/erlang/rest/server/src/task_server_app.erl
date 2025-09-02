-module(task_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start the task store
    task_store:init(),
    
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/tasks", task_list_handler, []},
            {"/api/tasks/:id", task_handler, []},
            {"/api/tasks/:id/status", task_status_handler, []}
        ]}
    ]),
    
    %% Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("🚀 Erlang Task REST API Server~n"),
    io:format("📍 Listening on http://localhost:8080~n"),
    io:format("🔍 Health check: http://localhost:8080/health~n~n"),
    
    task_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http),
    ok.