-module(task_store).
-behaviour(gen_server).

%% API
-export([start_link/0, init/0, create_task/1, get_task/1, update_task/2, 
         update_task_status/2, delete_task/1, list_tasks/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(task, {
    id :: binary(),
    title :: binary(),
    description :: binary() | undefined,
    status :: pending | in_progress | completed | cancelled,
    priority :: low | medium | high | urgent,
    tags :: [binary()],
    assigned_to :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer()
}).

-record(state, {
    tasks :: #{binary() => #task{}},
    counter :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    ets:new(tasks, [named_table, public, {keypos, #task.id}]),
    %% Add sample tasks
    create_task(#{
        <<"title">> => <<"Learn Erlang OTP">>,
        <<"description">> => <<"Master OTP principles and patterns">>,
        <<"priority">> => <<"high">>,
        <<"tags">> => [<<"erlang">>, <<"otp">>, <<"learning">>],
        <<"assigned_to">> => <<"developer">>
    }),
    create_task(#{
        <<"title">> => <<"Build REST API">>,
        <<"description">> => <<"Create REST API with Cowboy">>,
        <<"priority">> => <<"urgent">>,
        <<"tags">> => [<<"erlang">>, <<"api">>, <<"rest">>],
        <<"assigned_to">> => <<"backend-team">>
    }),
    create_task(#{
        <<"title">> => <<"Write Tests">>,
        <<"description">> => <<"Add EUnit and Common Test suites">>,
        <<"priority">> => <<"medium">>,
        <<"tags">> => [<<"testing">>, <<"quality">>]
    }),
    ok.

create_task(TaskData) ->
    gen_server:call(?MODULE, {create_task, TaskData}).

get_task(Id) ->
    gen_server:call(?MODULE, {get_task, Id}).

update_task(Id, Updates) ->
    gen_server:call(?MODULE, {update_task, Id, Updates}).

update_task_status(Id, Status) ->
    gen_server:call(?MODULE, {update_task_status, Id, Status}).

delete_task(Id) ->
    gen_server:call(?MODULE, {delete_task, Id}).

list_tasks(Filters) ->
    gen_server:call(?MODULE, {list_tasks, Filters}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{tasks = #{}, counter = 0}}.

handle_call({create_task, TaskData}, _From, State = #state{tasks = Tasks, counter = Counter}) ->
    NewCounter = Counter + 1,
    Id = list_to_binary("task-" ++ integer_to_list(NewCounter)),
    Now = erlang:system_time(second),
    
    Task = #task{
        id = Id,
        title = maps:get(<<"title">>, TaskData),
        description = maps:get(<<"description">>, TaskData, undefined),
        status = binary_to_status(maps:get(<<"status">>, TaskData, <<"pending">>)),
        priority = binary_to_priority(maps:get(<<"priority">>, TaskData, <<"medium">>)),
        tags = maps:get(<<"tags">>, TaskData, []),
        assigned_to = maps:get(<<"assigned_to">>, TaskData, undefined),
        created_at = Now,
        updated_at = Now
    },
    
    ets:insert(tasks, Task),
    NewTasks = maps:put(Id, Task, Tasks),
    {reply, {ok, task_to_map(Task)}, State#state{tasks = NewTasks, counter = NewCounter}};

handle_call({get_task, Id}, _From, State) ->
    case ets:lookup(tasks, Id) of
        [Task] -> {reply, {ok, task_to_map(Task)}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({update_task, Id, Updates}, _From, State = #state{tasks = Tasks}) ->
    case ets:lookup(tasks, Id) of
        [Task] ->
            Now = erlang:system_time(second),
            UpdatedTask = update_task_fields(Task, Updates, Now),
            ets:insert(tasks, UpdatedTask),
            NewTasks = maps:put(Id, UpdatedTask, Tasks),
            {reply, {ok, task_to_map(UpdatedTask)}, State#state{tasks = NewTasks}};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_task_status, Id, Status}, _From, State = #state{tasks = Tasks}) ->
    case ets:lookup(tasks, Id) of
        [Task] ->
            Now = erlang:system_time(second),
            UpdatedTask = Task#task{
                status = binary_to_status(Status),
                updated_at = Now
            },
            ets:insert(tasks, UpdatedTask),
            NewTasks = maps:put(Id, UpdatedTask, Tasks),
            {reply, {ok, task_to_map(UpdatedTask)}, State#state{tasks = NewTasks}};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_task, Id}, _From, State = #state{tasks = Tasks}) ->
    case ets:lookup(tasks, Id) of
        [_Task] ->
            ets:delete(tasks, Id),
            NewTasks = maps:remove(Id, Tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({list_tasks, Filters}, _From, State) ->
    AllTasks = ets:tab2list(tasks),
    FilteredTasks = filter_tasks(AllTasks, Filters),
    TaskMaps = [task_to_map(Task) || Task <- FilteredTasks],
    {reply, {ok, TaskMaps}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

task_to_map(#task{
    id = Id,
    title = Title,
    description = Description,
    status = Status,
    priority = Priority,
    tags = Tags,
    assigned_to = AssignedTo,
    created_at = CreatedAt,
    updated_at = UpdatedAt
}) ->
    Map = #{
        <<"id">> => Id,
        <<"title">> => Title,
        <<"status">> => status_to_binary(Status),
        <<"priority">> => priority_to_binary(Priority),
        <<"tags">> => Tags,
        <<"created_at">> => CreatedAt,
        <<"updated_at">> => UpdatedAt
    },
    Map1 = case Description of
        undefined -> Map;
        _ -> Map#{<<"description">> => Description}
    end,
    case AssignedTo of
        undefined -> Map1;
        _ -> Map1#{<<"assigned_to">> => AssignedTo}
    end.

update_task_fields(Task, Updates, Now) ->
    Task1 = case maps:get(<<"title">>, Updates, undefined) of
        undefined -> Task;
        Title -> Task#task{title = Title}
    end,
    Task2 = case maps:get(<<"description">>, Updates, undefined) of
        undefined -> Task1;
        Desc -> Task1#task{description = Desc}
    end,
    Task3 = case maps:get(<<"status">>, Updates, undefined) of
        undefined -> Task2;
        Status -> Task2#task{status = binary_to_status(Status)}
    end,
    Task4 = case maps:get(<<"priority">>, Updates, undefined) of
        undefined -> Task3;
        Priority -> Task3#task{priority = binary_to_priority(Priority)}
    end,
    Task5 = case maps:get(<<"tags">>, Updates, undefined) of
        undefined -> Task4;
        Tags -> Task4#task{tags = Tags}
    end,
    Task6 = case maps:get(<<"assigned_to">>, Updates, undefined) of
        undefined -> Task5;
        AssignedTo -> Task5#task{assigned_to = AssignedTo}
    end,
    Task6#task{updated_at = Now}.

filter_tasks(Tasks, Filters) ->
    StatusFilter = maps:get(<<"status">>, Filters, undefined),
    AssignedToFilter = maps:get(<<"assigned_to">>, Filters, undefined),
    
    lists:filter(fun(Task) ->
        StatusMatch = case StatusFilter of
            undefined -> true;
            Status -> Task#task.status == binary_to_status(Status)
        end,
        AssignedMatch = case AssignedToFilter of
            undefined -> true;
            AssignedTo -> Task#task.assigned_to == AssignedTo
        end,
        StatusMatch andalso AssignedMatch
    end, Tasks).

binary_to_status(<<"pending">>) -> pending;
binary_to_status(<<"in-progress">>) -> in_progress;
binary_to_status(<<"in_progress">>) -> in_progress;
binary_to_status(<<"completed">>) -> completed;
binary_to_status(<<"cancelled">>) -> cancelled;
binary_to_status(_) -> pending.

status_to_binary(pending) -> <<"pending">>;
status_to_binary(in_progress) -> <<"in-progress">>;
status_to_binary(completed) -> <<"completed">>;
status_to_binary(cancelled) -> <<"cancelled">>.

binary_to_priority(<<"low">>) -> low;
binary_to_priority(<<"medium">>) -> medium;
binary_to_priority(<<"high">>) -> high;
binary_to_priority(<<"urgent">>) -> urgent;
binary_to_priority(_) -> medium.

priority_to_binary(low) -> <<"low">>;
priority_to_binary(medium) -> <<"medium">>;
priority_to_binary(high) -> <<"high">>;
priority_to_binary(urgent) -> <<"urgent">>.