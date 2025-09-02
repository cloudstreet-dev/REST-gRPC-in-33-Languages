-module(task_store).
-behaviour(gen_server).

%% API
-export([start_link/0,
         list_tasks/0,
         get_task/1,
         create_task/1,
         update_task/3,
         delete_task/1,
         register_watcher/1,
         unregister_watcher/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    tasks = #{},
    watchers = [],
    counter = 0
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_tasks() ->
    gen_server:call(?MODULE, list_tasks).

get_task(Id) ->
    gen_server:call(?MODULE, {get_task, Id}).

create_task(Task) ->
    gen_server:call(?MODULE, {create_task, Task}).

update_task(Id, Updates, UpdateMask) ->
    gen_server:call(?MODULE, {update_task, Id, Updates, UpdateMask}).

delete_task(Id) ->
    gen_server:call(?MODULE, {delete_task, Id}).

register_watcher(Pid) ->
    gen_server:cast(?MODULE, {register_watcher, Pid}).

unregister_watcher(Pid) ->
    gen_server:cast(?MODULE, {unregister_watcher, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(list_tasks, _From, #state{tasks = Tasks} = State) ->
    TaskList = maps:values(Tasks),
    {reply, TaskList, State};

handle_call({get_task, Id}, _From, #state{tasks = Tasks} = State) ->
    case maps:find(Id, Tasks) of
        {ok, Task} ->
            {reply, {ok, Task}, State};
        error ->
            {reply, not_found, State}
    end;

handle_call({create_task, TaskData}, _From, 
            #state{tasks = Tasks, counter = Counter, watchers = Watchers} = State) ->
    Id = list_to_binary("task-" ++ integer_to_list(Counter + 1)),
    Now = erlang:system_time(microsecond),
    
    Task = TaskData#{
        id => Id,
        status => maps:get(status, TaskData, 1),  %% PENDING
        priority => maps:get(priority, TaskData, 2),  %% MEDIUM
        tags => maps:get(tags, TaskData, []),
        created_at => Now,
        updated_at => Now
    },
    
    NewTasks = maps:put(Id, Task, Tasks),
    
    %% Notify watchers
    notify_watchers(Watchers, {created, Task}),
    
    {reply, {ok, Task}, State#state{
        tasks = NewTasks,
        counter = Counter + 1
    }};

handle_call({update_task, Id, Updates, UpdateMask}, _From,
            #state{tasks = Tasks, watchers = Watchers} = State) ->
    case maps:find(Id, Tasks) of
        {ok, Task} ->
            UpdatedTask = apply_updates(Task, Updates, UpdateMask),
            NewTasks = maps:put(Id, UpdatedTask, Tasks),
            
            %% Notify watchers
            notify_watchers(Watchers, {updated, UpdatedTask}),
            
            {reply, {ok, UpdatedTask}, State#state{tasks = NewTasks}};
        error ->
            {reply, not_found, State}
    end;

handle_call({delete_task, Id}, _From,
            #state{tasks = Tasks, watchers = Watchers} = State) ->
    case maps:find(Id, Tasks) of
        {ok, Task} ->
            NewTasks = maps:remove(Id, Tasks),
            
            %% Notify watchers
            notify_watchers(Watchers, {deleted, Task}),
            
            {reply, ok, State#state{tasks = NewTasks}};
        error ->
            {reply, not_found, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register_watcher, Pid}, #state{watchers = Watchers} = State) ->
    monitor(process, Pid),
    {noreply, State#state{watchers = [Pid | Watchers]}};

handle_cast({unregister_watcher, Pid}, #state{watchers = Watchers} = State) ->
    {noreply, State#state{watchers = lists:delete(Pid, Watchers)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, 
            #state{watchers = Watchers} = State) ->
    {noreply, State#state{watchers = lists:delete(Pid, Watchers)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

apply_updates(Task, Updates, []) ->
    %% No update mask - replace everything except ID and timestamps
    maps:merge(Task, Updates#{
        id => maps:get(id, Task),
        created_at => maps:get(created_at, Task),
        updated_at => erlang:system_time(microsecond)
    });
apply_updates(Task, Updates, UpdateMask) ->
    %% Apply only specified fields
    UpdatedFields = lists:foldl(fun(Field, Acc) ->
        FieldAtom = binary_to_atom(Field, utf8),
        case maps:find(FieldAtom, Updates) of
            {ok, Value} -> maps:put(FieldAtom, Value, Acc);
            error -> Acc
        end
    end, #{}, UpdateMask),
    
    maps:merge(Task, UpdatedFields#{
        updated_at => erlang:system_time(microsecond)
    }).

notify_watchers(Watchers, {EventType, Task}) ->
    Event = #{
        event_type => event_type_to_enum(EventType),
        task => Task,
        timestamp => erlang:system_time(microsecond)
    },
    
    lists:foreach(fun(Pid) ->
        Pid ! {task_event, Event}
    end, Watchers).

event_type_to_enum(created) -> 1;  %% EVENT_TYPE_CREATED
event_type_to_enum(updated) -> 2;  %% EVENT_TYPE_UPDATED
event_type_to_enum(deleted) -> 3;  %% EVENT_TYPE_DELETED
event_type_to_enum(status_changed) -> 4.  %% EVENT_TYPE_STATUS_CHANGED