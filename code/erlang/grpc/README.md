# Erlang gRPC Implementation

This directory contains a gRPC implementation in Erlang using grpcbox.

## Features

- **grpcbox** for Erlang gRPC support
- **gen_server** for state management
- **Streaming support** for server and bidirectional streams
- **OTP supervision** for fault tolerance
- **Pattern matching** for request handling
- **Process-based concurrency** for scalability

## Prerequisites

- Erlang/OTP 24 or higher
- rebar3

Install rebar3:
```bash
# macOS
brew install rebar3

# Linux
wget https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
sudo mv rebar3 /usr/local/bin/

# Verify installation
rebar3 version
```

## Protocol Buffers

The implementation uses grpcbox's built-in protobuf support. Proto files should be placed in `../../shared/protos/`.

## Server

The server implements all gRPC service methods using OTP patterns:

### Running the Server

```bash
cd server
rebar3 compile
rebar3 shell
```

Or use the provided script:
```bash
./run-server.sh
```

### Service Implementation

```erlang
%% Unary RPC
get_task(#{id := Id}, _Stream) ->
    case task_store:get_task(Id) of
        {ok, Task} ->
            {ok, task_to_proto(Task), #{}};
        not_found ->
            {error, {?GRPC_STATUS_NOT_FOUND, <<"Task not found">>}}
    end.

%% Server streaming
list_tasks(Request, Stream) ->
    Tasks = task_store:list_tasks(),
    FilteredTasks = filter_tasks(Tasks, Request),
    
    lists:foreach(fun(Task) ->
        grpcbox_stream:send(false, task_to_proto(Task), Stream)
    end, FilteredTasks),
    
    {ok, Stream}.

%% Bidirectional streaming
watch_tasks(Stream) ->
    EventPid = spawn_link(fun() -> watch_loop(Stream) end),
    task_store:register_watcher(EventPid),
    handle_watch_requests(Stream, EventPid).
```

## Architecture

### OTP Application Structure

```
task_grpc_server
├── task_grpc_server_app     (Application callback)
├── task_grpc_server_sup      (Supervisor)
├── task_service              (gRPC service implementation)
└── task_store               (gen_server for state)
```

### State Management with gen_server

```erlang
-module(task_store).
-behaviour(gen_server).

-record(state, {
    tasks = #{},
    watchers = [],
    counter = 0
}).

handle_call({create_task, TaskData}, _From, State) ->
    Task = create_task_internal(TaskData, State),
    NewState = add_task(Task, State),
    notify_watchers(State#state.watchers, {created, Task}),
    {reply, {ok, Task}, NewState}.
```

### Streaming Support

Server streaming example:
```erlang
list_tasks(Request, Stream) ->
    spawn(fun() ->
        lists:foreach(fun(Task) ->
            timer:sleep(100),  % Simulate work
            grpcbox_stream:send(false, Task, Stream)
        end, get_filtered_tasks(Request))
    end),
    {ok, Stream}.
```

Bidirectional streaming:
```erlang
watch_tasks(Stream) ->
    spawn(fun() ->
        handle_incoming(Stream),
        handle_outgoing(Stream)
    end),
    {ok, Stream}.

handle_incoming(Stream) ->
    case grpcbox_stream:recv(Stream) of
        {ok, Request} ->
            process_request(Request),
            handle_incoming(Stream);
        eof ->
            ok
    end.
```

## Error Handling

gRPC status codes are properly mapped:

```erlang
-include_lib("grpcbox/include/grpcbox.hrl").

handle_error(not_found) ->
    {error, {?GRPC_STATUS_NOT_FOUND, <<"Resource not found">>}};
handle_error(invalid_argument) ->
    {error, {?GRPC_STATUS_INVALID_ARGUMENT, <<"Invalid input">>}};
handle_error(unauthenticated) ->
    {error, {?GRPC_STATUS_UNAUTHENTICATED, <<"Authentication required">>}}.
```

## Testing

```erlang
-module(task_service_tests).
-include_lib("eunit/include/eunit.hrl").

create_task_test() ->
    {ok, _} = task_store:start_link(),
    
    Request = #{task => #{
        title => <<"Test Task">>,
        priority => 3
    }},
    
    {ok, Task, _} = task_service:create_task(Request, undefined),
    ?assertEqual(<<"Test Task">>, maps:get(title, Task)),
    ?assertEqual(1, maps:get(status, Task)).  % PENDING

streaming_test() ->
    MockStream = self(),
    task_service:list_tasks(#{}, MockStream),
    
    receive
        {send, Task} ->
            ?assert(is_map(Task))
    after 1000 ->
        ?assert(false)
    end.
```

## Configuration

Server configuration in `config/sys.config`:

```erlang
[
    {grpcbox, [
        {servers, [
            #{grpc_opts => #{
                service_protos => [tasks_pb],
                services => #{'tasks.v1.TaskService' => task_service}
              },
              listen_opts => #{
                port => 50051,
                ip => {0, 0, 0, 0}
              }}
        ]}
    ]}
].
```

## Deployment

### Release with rebar3

```bash
cd server
rebar3 release
_build/default/rel/task_grpc_server/bin/task_grpc_server start
```

### Docker

```dockerfile
FROM erlang:24-alpine

RUN apk add --no-cache git

WORKDIR /app

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Copy and build
COPY . .
RUN cd server && rebar3 release

EXPOSE 50051

CMD ["server/_build/default/rel/task_grpc_server/bin/task_grpc_server", "foreground"]
```

## Best Practices

1. **Use gen_server for state** - Provides fault tolerance and clean API
2. **Leverage OTP supervision** - Automatic restart on failures
3. **Pattern match in function heads** - Clear and efficient
4. **Use records for complex data** - Better than deep maps
5. **Monitor client processes** - Clean up on disconnection
6. **Batch operations when possible** - Reduce lock contention

## Dependencies

- `grpcbox` - Erlang gRPC implementation
- `jsx` - JSON encoding/decoding (for debugging)