# Chapter 27: Erlang - Concurrent, Fault-Tolerant Programming

## Introduction

Erlang represents a paradigm shift in how we think about concurrent and distributed systems. Created by Joe Armstrong, Robert Virding, and Mike Williams at Ericsson in 1986, Erlang was designed for building massively concurrent, fault-tolerant telecommunication systems. Its "let it crash" philosophy and actor model have influenced modern languages and frameworks, making it the foundation for systems that require 99.9999999% uptime (nine nines).

In this chapter, we'll implement our task management REST API using Erlang and Cowboy, exploring how Erlang's unique approach to concurrency, fault tolerance, and distributed computing creates systems that can run for years without downtime.

## Why Erlang?

Erlang brings unique capabilities to systems programming:

1. **Actor Model**: Lightweight processes with message passing
2. **Fault Tolerance**: Supervision trees and "let it crash" philosophy
3. **Hot Code Swapping**: Update running systems without downtime
4. **Distributed Computing**: Built-in support for distributed systems
5. **Soft Real-Time**: Predictable latency for real-time systems
6. **Pattern Matching**: Powerful control flow through pattern matching

## Setting Up Erlang

Install Erlang/OTP and rebar3:

```bash
# macOS
brew install erlang rebar3

# Ubuntu/Debian
apt-get install erlang rebar3

# From source (latest version)
wget https://github.com/erlang/otp/releases/download/OTP-26.0/otp_src_26.0.tar.gz
tar -xzf otp_src_26.0.tar.gz
cd otp_src_26.0
./configure && make && sudo make install

# Install rebar3
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/
```

## Understanding Erlang's Philosophy

Before diving into the implementation, let's understand Erlang's core principles:

### Everything is a Process

```erlang
%% Spawn a new process
Pid = spawn(fun() -> 
    receive
        {hello, From} ->
            From ! {reply, "Hello back!"}
    end
end),

%% Send message to process
Pid ! {hello, self()},

%% Receive reply
receive
    {reply, Message} ->
        io:format("Got: ~s~n", [Message])
end.
```

### Let It Crash

```erlang
%% Don't do defensive programming
%% BAD - Defensive approach
divide(A, B) ->
    if
        B == 0 -> {error, division_by_zero};
        true -> {ok, A / B}
    end.

%% GOOD - Let it crash
divide(A, B) -> A / B.

%% Supervisor will restart on crash
```

### Immutable Data

```erlang
%% All data is immutable
List1 = [1, 2, 3],
List2 = [0 | List1],  % Creates new list [0, 1, 2, 3]
%% List1 is still [1, 2, 3]

%% Records are immutable
Task = #task{id = 1, title = "Learn Erlang"},
UpdatedTask = Task#task{status = completed},
%% Task is unchanged
```

## Implementing the REST API Server

Let's build our task management server using OTP principles:

### Application Structure

```erlang
%% task_server.app.src
{application, task_server, [
    {description, "Task Management REST API"},
    {vsn, "1.0.0"},
    {mod, {task_server_app, []}},
    {applications, [
        kernel,
        stdlib,
        cowboy,
        jiffy
    ]},
    {env, []},
    {modules, []}
]}.
```

### OTP Application

```erlang
-module(task_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/tasks", task_list_handler, []},
            {"/api/tasks/:id", task_handler, []},
            {"/api/tasks/:id/status", task_status_handler, []}
        ]}
    ]),
    
    %% Start HTTP server
    {ok, _} = cowboy:start_clear(http,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    %% Start supervisor
    task_server_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http).
```

### Supervisor

```erlang
-module(task_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,  % Restart strategy
        intensity => 10,           % Max restarts
        period => 10               % Time window
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
```

### GenServer for State Management

```erlang
-module(task_store).
-behaviour(gen_server).

%% API
-export([start_link/0, create_task/1, get_task/1, 
         update_task/2, delete_task/1, list_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2]).

-record(task, {
    id :: binary(),
    title :: binary(),
    description :: binary() | undefined,
    status :: atom(),
    priority :: atom(),
    tags :: [binary()],
    assigned_to :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer()
}).

-record(state, {
    tasks :: #{binary() => #task{}},
    counter :: integer()
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_task(TaskData) ->
    gen_server:call(?MODULE, {create_task, TaskData}).

get_task(Id) ->
    gen_server:call(?MODULE, {get_task, Id}).

%%% gen_server Callbacks

init([]) ->
    %% Initialize ETS table for fast lookups
    ets:new(tasks, [named_table, public, {keypos, #task.id}]),
    {ok, #state{tasks = #{}, counter = 0}}.

handle_call({create_task, Data}, _From, State) ->
    #state{tasks = Tasks, counter = Counter} = State,
    NewCounter = Counter + 1,
    Id = list_to_binary("task-" ++ integer_to_list(NewCounter)),
    
    Task = #task{
        id = Id,
        title = maps:get(<<"title">>, Data),
        status = pending,
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second)
    },
    
    ets:insert(tasks, Task),
    NewTasks = maps:put(Id, Task, Tasks),
    
    {reply, {ok, task_to_map(Task)}, 
     State#state{tasks = NewTasks, counter = NewCounter}};

handle_call({get_task, Id}, _From, State) ->
    case ets:lookup(tasks, Id) of
        [Task] -> {reply, {ok, task_to_map(Task)}, State};
        [] -> {reply, {error, not_found}, State}
    end.
```

### HTTP Request Handlers

```erlang
-module(task_list_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    %% Get query parameters
    Qs = cowboy_req:parse_qs(Req0),
    Filters = parse_filters(Qs),
    
    {ok, Tasks} = task_store:list_tasks(Filters),
    Response = #{
        <<"tasks">> => Tasks,
        <<"total_count">> => length(Tasks)
    },
    
    Req = cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Response), 
        Req0),
    
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try jiffy:decode(Body, [return_maps]) of
        TaskData ->
            case task_store:create_task(TaskData) of
                {ok, Task} ->
                    Req = cowboy_req:reply(201,
                        #{<<"content-type">> => <<"application/json">>},
                        jiffy:encode(Task),
                        Req1);
                {error, Reason} ->
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jiffy:encode(#{<<"error">> => Reason}),
                        Req1)
            end
    catch
        _:_ ->
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                jiffy:encode(#{<<"error">> => <<"Invalid JSON">>}),
                Req1)
    end,
    
    {ok, Req, State}.
```

## Pattern Matching and Control Flow

Erlang's pattern matching provides elegant control flow:

### Function Clauses

```erlang
%% Multiple function clauses with pattern matching
process_task({create, Data}) ->
    create_task(Data);
process_task({update, Id, Data}) ->
    update_task(Id, Data);
process_task({delete, Id}) ->
    delete_task(Id);
process_task(_) ->
    {error, unknown_operation}.

%% Guards for additional conditions
validate_priority(Priority) when Priority =:= low;
                                 Priority =:= medium;
                                 Priority =:= high;
                                 Priority =:= urgent ->
    {ok, Priority};
validate_priority(_) ->
    {error, invalid_priority}.
```

### Case Expressions

```erlang
handle_response(Response) ->
    case Response of
        {ok, Data} ->
            {200, encode(Data)};
        {error, not_found} ->
            {404, encode(#{error => "Not found"})};
        {error, {validation, Errors}} ->
            {400, encode(#{error => "Validation failed", 
                          details => Errors})};
        {error, _} ->
            {500, encode(#{error => "Internal server error"})}
    end.
```

## Concurrency and Message Passing

Erlang's actor model provides true concurrency:

### Spawning Processes

```erlang
%% Spawn a process for each task operation
handle_concurrent_updates(TaskIds, Updates) ->
    %% Spawn a process for each update
    Pids = [spawn_monitor(fun() -> 
        Result = update_task(Id, Updates),
        exit({Id, Result})
    end) || Id <- TaskIds],
    
    %% Collect results
    collect_results(Pids, []).

collect_results([], Results) ->
    Results;
collect_results([{Pid, Ref} | Rest], Results) ->
    receive
        {'DOWN', Ref, process, Pid, {Id, Result}} ->
            collect_results(Rest, [{Id, Result} | Results]);
        {'DOWN', Ref, process, Pid, Reason} ->
            collect_results(Rest, [{error, Reason} | Results])
    after 5000 ->
        collect_results(Rest, [{error, timeout} | Results])
    end.
```

### Message Passing Patterns

```erlang
%% Request-Reply pattern
request_reply(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {request, self(), Ref, Request},
    receive
        {reply, Ref, Response} ->
            Response
    after 5000 ->
        {error, timeout}
    end.

%% Publish-Subscribe pattern
-module(event_manager).
-behaviour(gen_event).

notify_task_created(Task) ->
    gen_event:notify(task_events, {task_created, Task}).

subscribe_to_events(Handler) ->
    gen_event:add_handler(task_events, Handler, []).
```

## Fault Tolerance

### Supervision Trees

```erlang
%% Complex supervision tree
-module(app_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{strategy => one_for_one},
    
    ChildSpecs = [
        %% Database connection pool
        #{id => db_pool_sup,
          start => {db_pool_sup, start_link, []},
          type => supervisor},
        
        %% Cache supervisor
        #{id => cache_sup,
          start => {cache_sup, start_link, []},
          type => supervisor,
          restart => permanent},
        
        %% Task workers supervisor
        #{id => task_workers_sup,
          start => {task_workers_sup, start_link, []},
          type => supervisor,
          restart => permanent}
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
```

### Error Handling Strategies

```erlang
%% Let it crash with proper cleanup
process_file(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    %% Link ensures File is closed if process crashes
    link(File),
    process_file_contents(File).

%% Trap exits for cleanup
init(State) ->
    process_flag(trap_exit, true),
    {ok, State}.

terminate(Reason, State) ->
    %% Cleanup code here
    cleanup(State),
    ok.
```

## Distributed Erlang

### Building Distributed Systems

```erlang
%% Start distributed nodes
%% Node 1: erl -name api@host1 -setcookie secret
%% Node 2: erl -name db@host2 -setcookie secret

%% Connect nodes
connect_cluster() ->
    Nodes = ['api@host1', 'db@host2', 'cache@host3'],
    [net_adm:ping(Node) || Node <- Nodes].

%% Distributed calls
distributed_get_task(Id) ->
    rpc:call('db@host2', task_store, get_task, [Id]).

%% Global process registration
global:register_name(task_manager, self()),
Pid = global:whereis_name(task_manager).
```

### Distributed Task Processing

```erlang
%% Distribute work across nodes
-module(distributed_processor).

process_tasks(Tasks) ->
    Nodes = [node() | nodes()],
    TaskChunks = split_tasks(Tasks, length(Nodes)),
    
    %% Spawn work on each node
    Results = pmap(fun({Node, Chunk}) ->
        rpc:async_call(Node, task_processor, process_chunk, [Chunk])
    end, lists:zip(Nodes, TaskChunks)),
    
    %% Collect results
    [rpc:yield(Key) || Key <- Results].

split_tasks(Tasks, N) ->
    Len = length(Tasks),
    ChunkSize = Len div N + 1,
    split_into_chunks(Tasks, ChunkSize).
```

## Testing Strategies

### EUnit for Unit Testing

```erlang
-module(task_store_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
setup() ->
    task_store:start_link(),
    ok.

teardown(_) ->
    gen_server:stop(task_store).

%% Test cases
create_task_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_) ->
         [
          ?_test(begin
              Data = #{<<"title">> => <<"Test">>},
              {ok, Task} = task_store:create_task(Data),
              ?assertEqual(<<"Test">>, maps:get(<<"title">>, Task))
          end),
          ?_test(begin
              %% Test validation
              Data = #{},  % Missing title
              Result = task_store:create_task(Data),
              ?assertMatch({error, _}, Result)
          end)
         ]
     end}.

%% Property-based testing with PropEr
prop_task_id_unique() ->
    ?FORALL(Titles, list(binary()),
        begin
            Tasks = [task_store:create_task(#{<<"title">> => T}) 
                    || T <- Titles],
            Ids = [maps:get(<<"id">>, Task) 
                  || {ok, Task} <- Tasks],
            length(Ids) =:= length(lists:usort(Ids))
        end).
```

### Common Test for Integration Testing

```erlang
-module(api_SUITE).
-compile(export_all).

all() -> [test_crud_operations, test_concurrent_access].

init_per_suite(Config) ->
    application:ensure_all_started(task_server),
    Config.

test_crud_operations(Config) ->
    %% Create
    {ok, 201, _, Ref1} = hackney:post(
        "http://localhost:8080/api/tasks",
        [{<<"Content-Type">>, <<"application/json">>}],
        jiffy:encode(#{title => "Test"}),
        []
    ),
    {ok, Body1} = hackney:body(Ref1),
    Task = jiffy:decode(Body1, [return_maps]),
    Id = maps:get(<<"id">>, Task),
    
    %% Read
    {ok, 200, _, Ref2} = hackney:get(
        "http://localhost:8080/api/tasks/" ++ binary_to_list(Id)
    ),
    
    %% Update
    {ok, 200, _, Ref3} = hackney:put(
        "http://localhost:8080/api/tasks/" ++ binary_to_list(Id),
        [{<<"Content-Type">>, <<"application/json">>}],
        jiffy:encode(#{title => "Updated"}),
        []
    ),
    
    %% Delete
    {ok, 204, _, _} = hackney:delete(
        "http://localhost:8080/api/tasks/" ++ binary_to_list(Id)
    ).
```

## Performance Optimization

### ETS for Caching

```erlang
%% High-performance cache with ETS
-module(task_cache).

init() ->
    ets:new(cache, [
        named_table,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]).

get(Key) ->
    case ets:lookup(cache, Key) of
        [{_, Value, Expiry}] ->
            Now = erlang:system_time(second),
            if
                Now < Expiry -> {ok, Value};
                true -> 
                    ets:delete(cache, Key),
                    {error, expired}
            end;
        [] ->
            {error, not_found}
    end.

put(Key, Value, TTL) ->
    Expiry = erlang:system_time(second) + TTL,
    ets:insert(cache, {Key, Value, Expiry}).
```

### Process Pooling

```erlang
%% Worker pool for CPU-intensive tasks
-module(worker_pool).

init(PoolSize) ->
    Workers = [spawn_link(fun worker_loop/0) 
              || _ <- lists:seq(1, PoolSize)],
    register(worker_pool, self()),
    pool_loop(Workers, queue:new()).

pool_loop(Workers, Queue) ->
    receive
        {request, From, Fun} ->
            case Workers of
                [Worker | Rest] ->
                    Worker ! {work, From, Fun},
                    pool_loop(Rest, Queue);
                [] ->
                    NewQueue = queue:in({From, Fun}, Queue),
                    pool_loop(Workers, NewQueue)
            end;
        {worker_done, Worker} ->
            case queue:out(Queue) of
                {{value, {From, Fun}}, NewQueue} ->
                    Worker ! {work, From, Fun},
                    pool_loop(Workers, NewQueue);
                {empty, _} ->
                    pool_loop([Worker | Workers], Queue)
            end
    end.
```

## Best Practices

1. **Let It Crash**: Don't write defensive code, use supervisors
2. **Small Processes**: Many small processes instead of few large ones
3. **Immutable Data**: Leverage immutability for concurrency
4. **Message Passing**: No shared state between processes
5. **Supervision Trees**: Structure applications as supervision trees
6. **Hot Code Loading**: Design for code updates without downtime
7. **Pattern Matching**: Use pattern matching instead of conditionals

## gRPC Considerations

Erlang has limited but functional gRPC support through community libraries:

**Available Options**:
- **grpcbox**: A pure Erlang gRPC implementation that supports both client and server
- **grpc_client**: Erlang client for gRPC services
- Protocol buffer support via `gpb` (Google Protocol Buffers for Erlang)

**Implementation Challenges**:
- Less mature than gRPC support in mainstream languages
- Limited tooling for code generation from `.proto` files
- HTTP/2 support complexities in pure Erlang
- Manual mapping between Erlang records and protocol buffer messages

**Why Erlang Often Doesn't Need gRPC**:
- **Distributed Erlang**: Native clustering without serialization overhead
- **gen_server**: Built-in RPC-like communication patterns
- **Custom Protocols**: Easy to implement efficient binary protocols
- **Message Passing**: Actor model eliminates many RPC use cases

**When to Use gRPC with Erlang**:
- Interoperability with services written in other languages
- Compliance with organizational standards requiring gRPC
- Integration with existing gRPC infrastructure

For pure Erlang/OTP systems, the native distribution mechanisms often provide better performance and simpler debugging than gRPC. However, grpcbox provides a viable path when gRPC is required for external integration.

## Conclusion

Erlang's approach to building concurrent, fault-tolerant systems is unique and powerful. Its actor model, supervision trees, and "let it crash" philosophy create systems that can run for years without downtime. While the syntax might seem unusual at first, the conceptual simplicity and power of Erlang's model make it ideal for building reliable distributed systems.

The principles pioneered by Erlang have influenced many modern technologies, from Akka to Orleans, and languages like Elixir have brought Erlang's power to new audiences. Understanding Erlang is understanding the future of distributed systems.