# Erlang REST API Implementation

This directory contains a REST API implementation in Erlang using Cowboy, demonstrating fault-tolerant concurrent programming with OTP (Open Telecom Platform).

## Features

- **OTP Supervision Trees** for fault tolerance
- **gen_server** for stateful components
- **Cowboy 2.x** HTTP server
- **ETS tables** for fast in-memory storage
- **Pattern matching** for request routing
- **Hot code reloading** support

## Prerequisites

- Erlang/OTP 24 or higher
- rebar3 (Erlang build tool)

Install Erlang:
```bash
# macOS
brew install erlang rebar3

# Ubuntu/Debian
apt-get install erlang rebar3

# From source
wget https://github.com/erlang/otp/releases/download/OTP-26.0/otp_src_26.0.tar.gz
tar -xzf otp_src_26.0.tar.gz
cd otp_src_26.0
./configure && make && sudo make install
```

## Server

The server implements a complete REST API using Cowboy HTTP server and OTP principles.

### Running the Server

```bash
cd server
rebar3 compile
rebar3 shell --apps task_server
```

Or use the provided script:
```bash
./run-server.sh
```

The server will start on port 8080 by default.

### API Endpoints

- `GET /api/tasks` - List all tasks (with filtering)
- `GET /api/tasks/:id` - Get a specific task
- `POST /api/tasks` - Create a new task
- `PUT /api/tasks/:id` - Update a task
- `PATCH /api/tasks/:id/status` - Update task status
- `DELETE /api/tasks/:id` - Delete a task
- `GET /health` - Health check

### Query Parameters

- `status` - Filter by task status (pending, in-progress, completed, cancelled)
- `assigned_to` - Filter by assignee

## Client

The client provides a comprehensive SDK and demo for interacting with the REST API.

### Running the Client Demo

```bash
cd client
rebar3 compile
rebar3 escriptize
./_build/default/bin/task_client
```

Or use the provided script:
```bash
./run-client.sh
```

### Using the Client Library

```erlang
%% Start the application
application:ensure_all_started(hackney),

%% List all tasks
{ok, Tasks} = task_client:list_tasks(),

%% Create a task
NewTask = #{
    <<"title">> => <<"Learn Erlang">>,
    <<"priority">> => <<"high">>,
    <<"tags">> => [<<"erlang">>, <<"learning">>]
},
{ok, Task} = task_client:create_task(NewTask),

%% Get a task
TaskId = maps:get(<<"id">>, Task),
{ok, TaskDetail} = task_client:get_task(TaskId),

%% Update task
Updates = #{<<"title">> => <<"Master Erlang OTP">>},
{ok, Updated} = task_client:update_task(TaskId, Updates),

%% Update task status
{ok, _} = task_client:update_task_status(TaskId, <<"completed">>),

%% Delete task
ok = task_client:delete_task(TaskId).
```

## Architecture

### OTP Supervision Tree

```erlang
%% Application supervisor
task_server_sup
    └── task_store (gen_server)
```

### gen_server State Management

```erlang
-record(state, {
    tasks :: #{binary() => #task{}},
    counter :: integer()
}).

handle_call({create_task, TaskData}, _From, State) ->
    %% Process synchronous requests
    {reply, Result, NewState};

handle_cast(Msg, State) ->
    %% Process asynchronous messages
    {noreply, NewState}.
```

### Concurrent Request Handling

```erlang
%% Each HTTP request is handled in its own process
init(Req0, State) ->
    %% Process isolation ensures fault tolerance
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).
```

## Erlang Features Demonstrated

### Pattern Matching

```erlang
%% Function clauses with pattern matching
handle_request(<<"GET">>, Req, State) ->
    %% Handle GET request
    {ok, Req, State};

handle_request(<<"POST">>, Req, State) ->
    %% Handle POST request
    {ok, Req, State};

handle_request(_, Req, State) ->
    %% Handle other methods
    {ok, Req, State}.
```

### Message Passing

```erlang
%% Synchronous message (call)
gen_server:call(Pid, {create_task, Data}),

%% Asynchronous message (cast)
gen_server:cast(Pid, {update_cache, Data}),

%% Direct message send
Pid ! {notification, TaskCreated}.
```

### Process Supervision

```erlang
%% Supervisor specification
init([]) ->
    SupFlags = #{
        strategy => one_for_all,  % Restart all children if one dies
        intensity => 10,           % Max 10 restarts
        period => 10               % In 10 seconds
    },
    
    ChildSpecs = [
        #{
            id => task_store,
            start => {task_store, start_link, []},
            restart => permanent,     % Always restart
            shutdown => 5000,
            type => worker
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
```

### Error Handling

```erlang
%% Let it crash philosophy
case dangerous_operation() of
    {ok, Result} -> 
        process_result(Result);
    {error, Reason} ->
        %% Log and let supervisor restart
        error_logger:error_msg("Operation failed: ~p~n", [Reason]),
        exit(Reason)
end.

%% Try-catch for recoverable errors
try
    risky_operation()
catch
    Class:Reason:Stacktrace ->
        error_logger:error_msg("Caught ~p:~p~n~p~n", 
                              [Class, Reason, Stacktrace]),
        {error, Reason}
end.
```

## Testing

### EUnit Tests

```erlang
%% test/task_store_tests.erl
-module(task_store_tests).
-include_lib("eunit/include/eunit.hrl").

create_task_test() ->
    task_store:init(),
    TaskData = #{<<"title">> => <<"Test Task">>},
    {ok, Task} = task_store:create_task(TaskData),
    ?assertEqual(<<"Test Task">>, maps:get(<<"title">>, Task)).

task_lifecycle_test() ->
    task_store:init(),
    %% Create
    {ok, Task} = task_store:create_task(#{<<"title">> => <<"Test">>}),
    Id = maps:get(<<"id">>, Task),
    
    %% Read
    {ok, Retrieved} = task_store:get_task(Id),
    ?assertEqual(Task, Retrieved),
    
    %% Update
    {ok, Updated} = task_store:update_task(Id, #{<<"title">> => <<"Updated">>}),
    ?assertEqual(<<"Updated">>, maps:get(<<"title">>, Updated)),
    
    %% Delete
    ok = task_store:delete_task(Id),
    {error, not_found} = task_store:get_task(Id).
```

Run tests:
```bash
rebar3 eunit
```

### Common Test

```erlang
%% test/task_api_SUITE.erl
-module(task_api_SUITE).
-compile(export_all).

all() ->
    [test_create_task, test_list_tasks, test_update_task].

init_per_suite(Config) ->
    application:ensure_all_started(task_server),
    Config.

test_create_task(_Config) ->
    {ok, 201, _, _} = hackney:request(post, 
        "http://localhost:8080/api/tasks",
        [{<<"Content-Type">>, <<"application/json">>}],
        <<"{\"title\":\"Test Task\"}">>,
        []).
```

Run Common Test:
```bash
rebar3 ct
```

## Performance Optimization

### ETS for Fast Lookups

```erlang
%% Create ETS table for caching
ets:new(task_cache, [
    named_table,
    public,
    {read_concurrency, true},  % Optimize for concurrent reads
    {write_concurrency, true}   % Optimize for concurrent writes
]).

%% Fast O(1) lookup
case ets:lookup(task_cache, TaskId) of
    [{_, Task}] -> {ok, Task};
    [] -> {error, not_found}
end.
```

### Process Pooling

```erlang
%% Use poolboy for connection pooling
{ok, _} = poolboy:start_link([
    {name, {local, db_pool}},
    {worker_module, db_worker},
    {size, 10},          % 10 workers
    {max_overflow, 20}   % Allow 20 more under load
]).

%% Use a worker from the pool
poolboy:transaction(db_pool, fun(Worker) ->
    gen_server:call(Worker, {query, SQL})
end).
```

## Hot Code Reloading

```erlang
%% Compile and load new version
c(module_name).

%% Or reload all changed modules
l(module_name).

%% Check loaded versions
code:which(module_name).

%% Purge old code
code:purge(module_name).
```

## Distribution

### Starting Distributed Nodes

```bash
# Start node 1
erl -name node1@host1 -setcookie secret

# Start node 2
erl -name node2@host2 -setcookie secret

# Connect nodes
(node1@host1)> net_adm:ping('node2@host2').
pong

# Run distributed operations
(node1@host1)> rpc:call('node2@host2', module, function, [Args]).
```

## Best Practices

1. **Let it crash**: Design for failure, use supervisors
2. **Immutable data**: All data is immutable by default
3. **Message passing**: No shared state between processes
4. **Pattern matching**: Use pattern matching for control flow
5. **Small functions**: Keep functions small and focused
6. **Supervision trees**: Structure applications as supervision trees
7. **Named processes**: Register important processes for easy access

## Docker Support

```dockerfile
FROM erlang:26-alpine

RUN apk add --no-cache git build-base

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

WORKDIR /app
COPY . .

RUN cd server && rebar3 compile

EXPOSE 8080
CMD ["sh", "-c", "cd server && rebar3 shell --apps task_server"]
```

## Dependencies

### Server
- `cowboy` - HTTP server
- `jiffy` - JSON encoding/decoding
- `uuid` - UUID generation

### Client
- `hackney` - HTTP client
- `jiffy` - JSON encoding/decoding