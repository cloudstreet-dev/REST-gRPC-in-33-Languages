-module(task_service).

%% gRPC service callbacks
-export([list_tasks/2,
         get_task/2,
         create_task/2,
         update_task/2,
         delete_task/2,
         watch_tasks/1]).

-include_lib("grpcbox/include/grpcbox.hrl").

%% List tasks with optional filtering (server streaming)
list_tasks(Request, Stream) ->
    #{status := Status,
      assigned_to := AssignedTo,
      tags := Tags} = Request,
    
    %% Get all tasks from store
    Tasks = task_store:list_tasks(),
    
    %% Filter tasks
    FilteredTasks = filter_tasks(Tasks, Status, AssignedTo, Tags),
    
    %% Stream each task to client
    lists:foreach(fun(Task) ->
        grpcbox_stream:send(false, task_to_proto(Task), Stream)
    end, FilteredTasks),
    
    %% End the stream
    {ok, Stream}.

%% Get a single task by ID
get_task(#{id := Id}, _Stream) ->
    case task_store:get_task(Id) of
        {ok, Task} ->
            {ok, task_to_proto(Task), #{}};
        not_found ->
            {error, {?GRPC_STATUS_NOT_FOUND, <<"Task not found">>}}
    end.

%% Create a new task
create_task(#{task := TaskProto}, _Stream) ->
    Task = proto_to_task(TaskProto),
    {ok, CreatedTask} = task_store:create_task(Task),
    {ok, task_to_proto(CreatedTask), #{}}.

%% Update an existing task
update_task(#{task := TaskProto, update_mask := UpdateMask}, _Stream) ->
    #{id := Id} = TaskProto,
    Updates = proto_to_task(TaskProto),
    
    case task_store:update_task(Id, Updates, UpdateMask) of
        {ok, UpdatedTask} ->
            {ok, task_to_proto(UpdatedTask), #{}};
        not_found ->
            {error, {?GRPC_STATUS_NOT_FOUND, <<"Task not found">>}}
    end.

%% Delete a task
delete_task(#{id := Id}, _Stream) ->
    case task_store:delete_task(Id) of
        ok ->
            {ok, #{}, #{}};
        not_found ->
            {error, {?GRPC_STATUS_NOT_FOUND, <<"Task not found">>}}
    end.

%% Watch for task changes (bidirectional streaming)
watch_tasks(Stream) ->
    %% Register this stream for events
    EventPid = spawn_link(fun() -> watch_loop(Stream) end),
    task_store:register_watcher(EventPid),
    
    %% Process incoming watch requests
    handle_watch_requests(Stream, EventPid).

%% Internal functions

watch_loop(Stream) ->
    receive
        {task_event, Event} ->
            grpcbox_stream:send(false, event_to_proto(Event), Stream),
            watch_loop(Stream);
        stop ->
            ok
    end.

handle_watch_requests(Stream, EventPid) ->
    case grpcbox_stream:recv(Stream) of
        {ok, Request} ->
            process_watch_request(Request, EventPid),
            handle_watch_requests(Stream, EventPid);
        eof ->
            EventPid ! stop,
            task_store:unregister_watcher(EventPid),
            {ok, Stream};
        {error, _} = Error ->
            EventPid ! stop,
            task_store:unregister_watcher(EventPid),
            Error
    end.

process_watch_request(#{watch_all := true}, _EventPid) ->
    %% Watch all tasks - no filtering needed
    ok;
process_watch_request(#{task_ids := TaskIds}, EventPid) when TaskIds =/= [] ->
    %% Store filter for specific task IDs
    put({filter, EventPid}, {task_ids, TaskIds});
process_watch_request(#{assigned_to := AssignedTo}, EventPid) when AssignedTo =/= <<>> ->
    %% Store filter for assigned user
    put({filter, EventPid}, {assigned_to, AssignedTo}).

filter_tasks(Tasks, Status, AssignedTo, Tags) ->
    Tasks1 = case Status of
        0 -> Tasks;  %% UNSPECIFIED
        _ -> [T || T <- Tasks, maps:get(status, T) =:= Status]
    end,
    
    Tasks2 = case AssignedTo of
        <<>> -> Tasks1;
        _ -> [T || T <- Tasks1, maps:get(assigned_to, T) =:= AssignedTo]
    end,
    
    case Tags of
        [] -> Tasks2;
        _ -> [T || T <- Tasks2, has_all_tags(T, Tags)]
    end.

has_all_tags(Task, RequiredTags) ->
    TaskTags = maps:get(tags, Task, []),
    lists:all(fun(Tag) -> lists:member(Tag, TaskTags) end, RequiredTags).

%% Conversion functions

task_to_proto(Task) ->
    #{
        id => maps:get(id, Task),
        title => maps:get(title, Task),
        description => maps:get(description, Task, <<>>),
        status => maps:get(status, Task, 1),  %% Default to PENDING
        priority => maps:get(priority, Task, 2),  %% Default to MEDIUM
        tags => maps:get(tags, Task, []),
        created_by => maps:get(created_by, Task, <<>>),
        assigned_to => maps:get(assigned_to, Task, <<>>),
        created_at => timestamp_to_proto(maps:get(created_at, Task)),
        updated_at => timestamp_to_proto(maps:get(updated_at, Task)),
        due_date => timestamp_to_proto(maps:get(due_date, Task, undefined)),
        completed_at => timestamp_to_proto(maps:get(completed_at, Task, undefined))
    }.

proto_to_task(Proto) ->
    #{
        id => maps:get(id, Proto, undefined),
        title => maps:get(title, Proto),
        description => maps:get(description, Proto, <<>>),
        status => maps:get(status, Proto, 1),
        priority => maps:get(priority, Proto, 2),
        tags => maps:get(tags, Proto, []),
        created_by => maps:get(created_by, Proto, <<>>),
        assigned_to => maps:get(assigned_to, Proto, <<>>),
        created_at => proto_to_timestamp(maps:get(created_at, Proto, undefined)),
        updated_at => proto_to_timestamp(maps:get(updated_at, Proto, undefined)),
        due_date => proto_to_timestamp(maps:get(due_date, Proto, undefined)),
        completed_at => proto_to_timestamp(maps:get(completed_at, Proto, undefined))
    }.

event_to_proto(#{event_type := Type, task := Task, timestamp := Timestamp}) ->
    #{
        event_type => Type,
        task => task_to_proto(Task),
        timestamp => timestamp_to_proto(Timestamp)
    }.

timestamp_to_proto(undefined) ->
    undefined;
timestamp_to_proto(Timestamp) ->
    Seconds = Timestamp div 1000000,
    Nanos = (Timestamp rem 1000000) * 1000,
    #{seconds => Seconds, nanos => Nanos}.

proto_to_timestamp(undefined) ->
    undefined;
proto_to_timestamp(#{seconds := Seconds, nanos := Nanos}) ->
    Seconds * 1000000 + Nanos div 1000.