-module(task_client).

-export([main/1]).
-export([list_tasks/0, list_tasks/1, create_task/1, get_task/1, 
         update_task/2, update_task_status/2, delete_task/1]).

-define(BASE_URL, "http://localhost:8080").

main(_Args) ->
    %% Start hackney
    application:ensure_all_started(hackney),
    
    print_banner(),
    timer:sleep(1000),
    run_demo(),
    
    %% Stop hackney
    application:stop(hackney),
    ok.

print_banner() ->
    io:format("╔════════════════════════════════════════════════╗~n"),
    io:format("║       Erlang Task Management REST Client       ║~n"),
    io:format("║            Testing API Operations              ║~n"),
    io:format("╚════════════════════════════════════════════════╝~n~n").

run_demo() ->
    %% 1. List all tasks
    io:format("1. Listing all tasks...~n"),
    case list_tasks() of
        {ok, Tasks} ->
            io:format("   Found ~p tasks~n", [length(Tasks)]),
            lists:foreach(fun(Task) ->
                Id = maps:get(<<"id">>, Task),
                Title = maps:get(<<"title">>, Task),
                Status = maps:get(<<"status">>, Task),
                io:format("   - [~s] ~s (~s)~n", [Id, Title, Status])
            end, Tasks);
        {error, Reason} ->
            io:format("   Error: ~p~n", [Reason])
    end,
    
    %% 2. Create a new task
    io:format("~n2. Creating a new task...~n"),
    NewTask = #{
        <<"title">> => <<"Learn Erlang concurrency">>,
        <<"description">> => <<"Master processes, messages, and supervision trees">>,
        <<"priority">> => <<"high">>,
        <<"tags">> => [<<"erlang">>, <<"concurrency">>, <<"otp">>],
        <<"assigned_to">> => <<"erlang-team">>
    },
    
    case create_task(NewTask) of
        {ok, CreatedTask} ->
            TaskId = maps:get(<<"id">>, CreatedTask),
            io:format("   Created task: ~s~n", [maps:get(<<"title">>, CreatedTask)]),
            io:format("   ID: ~s~n", [TaskId]),
            io:format("   Priority: ~s~n", [maps:get(<<"priority">>, CreatedTask)]),
            Tags = maps:get(<<"tags">>, CreatedTask),
            io:format("   Tags: ~s~n", [string:join([binary_to_list(T) || T <- Tags], ", ")]),
            
            %% 3. Get task details
            io:format("~n3. Getting task details...~n"),
            case get_task(TaskId) of
                {ok, TaskDetail} ->
                    io:format("   Title: ~s~n", [maps:get(<<"title">>, TaskDetail)]),
                    case maps:get(<<"description">>, TaskDetail, undefined) of
                        undefined -> ok;
                        Desc -> io:format("   Description: ~s~n", [Desc])
                    end,
                    io:format("   Status: ~s~n", [maps:get(<<"status">>, TaskDetail)]),
                    case maps:get(<<"assigned_to">>, TaskDetail, undefined) of
                        undefined -> ok;
                        AssignedTo -> io:format("   Assigned to: ~s~n", [AssignedTo])
                    end;
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end,
            
            %% 4. Update task status
            io:format("~n4. Updating task status to 'in-progress'...~n"),
            case update_task_status(TaskId, <<"in-progress">>) of
                {ok, UpdatedTask} ->
                    io:format("   Updated status to: ~s~n", [maps:get(<<"status">>, UpdatedTask)]);
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end,
            
            %% 5. Update task details
            io:format("~n5. Updating task details...~n"),
            Updates = #{
                <<"title">> => <<"Master Erlang/OTP design principles">>,
                <<"priority">> => <<"urgent">>
            },
            case update_task(TaskId, Updates) of
                {ok, UpdatedTask2} ->
                    io:format("   Updated title: ~s~n", [maps:get(<<"title">>, UpdatedTask2)]),
                    io:format("   Updated priority: ~s~n", [maps:get(<<"priority">>, UpdatedTask2)]);
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end,
            
            %% 6. Filter tasks by status
            io:format("~n6. Filtering tasks by status...~n"),
            case list_tasks(#{<<"status">> => <<"in-progress">>}) of
                {ok, FilteredTasks} ->
                    io:format("   Found ~p in-progress tasks~n", [length(FilteredTasks)]),
                    lists:foreach(fun(Task) ->
                        io:format("   - ~s~n", [maps:get(<<"title">>, Task)])
                    end, FilteredTasks);
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end,
            
            %% 7. Delete the task
            io:format("~n7. Deleting the task...~n"),
            case delete_task(TaskId) of
                ok ->
                    io:format("   Task deleted successfully~n");
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end,
            
            %% 8. Verify deletion
            io:format("~n8. Verifying deletion...~n"),
            case get_task(TaskId) of
                {ok, _} ->
                    io:format("   Error: Task still exists~n");
                {error, not_found} ->
                    io:format("   Task not found (as expected)~n");
                {error, Reason} ->
                    io:format("   Error: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("   Error: ~p~n", [Reason])
    end,
    
    io:format("~n✅ Demo completed successfully!~n").

%% API Functions

list_tasks() ->
    list_tasks(#{}).

list_tasks(Filters) ->
    QueryString = build_query_string(Filters),
    Url = ?BASE_URL ++ "/api/tasks" ++ QueryString,
    
    case hackney:request(get, Url, [], <<>>, []) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            Response = jiffy:decode(Body, [return_maps]),
            {ok, maps:get(<<"tasks">>, Response)};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

create_task(TaskData) ->
    Url = ?BASE_URL ++ "/api/tasks",
    Body = jiffy:encode(TaskData),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    
    case hackney:request(post, Url, Headers, Body, []) of
        {ok, 201, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, jiffy:decode(ResponseBody, [return_maps])};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {error, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

get_task(Id) ->
    Url = ?BASE_URL ++ "/api/tasks/" ++ binary_to_list(Id),
    
    case hackney:request(get, Url, [], <<>>, []) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, jiffy:decode(Body, [return_maps])};
        {ok, 404, _Headers, _ClientRef} ->
            {error, not_found};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

update_task(Id, Updates) ->
    Url = ?BASE_URL ++ "/api/tasks/" ++ binary_to_list(Id),
    Body = jiffy:encode(Updates),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    
    case hackney:request(put, Url, Headers, Body, []) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, jiffy:decode(ResponseBody, [return_maps])};
        {ok, 404, _Headers, _ClientRef} ->
            {error, not_found};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {error, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

update_task_status(Id, Status) ->
    Url = ?BASE_URL ++ "/api/tasks/" ++ binary_to_list(Id) ++ "/status",
    Body = jiffy:encode(#{<<"status">> => Status}),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    
    case hackney:request(patch, Url, Headers, Body, []) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {ok, jiffy:decode(ResponseBody, [return_maps])};
        {ok, 404, _Headers, _ClientRef} ->
            {error, not_found};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, ResponseBody} = hackney:body(ClientRef),
            {error, {StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, Reason}
    end.

delete_task(Id) ->
    Url = ?BASE_URL ++ "/api/tasks/" ++ binary_to_list(Id),
    
    case hackney:request(delete, Url, [], <<>>, []) of
        {ok, 204, _Headers, _ClientRef} ->
            ok;
        {ok, 404, _Headers, _ClientRef} ->
            {error, not_found};
        {ok, StatusCode, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, {StatusCode, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper Functions

build_query_string(Filters) when map_size(Filters) == 0 ->
    "";
build_query_string(Filters) ->
    Params = maps:fold(fun(K, V, Acc) ->
        Key = binary_to_list(K),
        Value = binary_to_list(V),
        [Key ++ "=" ++ Value | Acc]
    end, [], Filters),
    "?" ++ string:join(Params, "&").