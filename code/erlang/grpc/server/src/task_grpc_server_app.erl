-module(task_grpc_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start the task store
    task_store:start_link(),
    
    %% Configure gRPC server
    Routes = [{'tasks.v1.TaskService', task_service}],
    
    %% Start gRPC server on port 50051
    {ok, _} = grpcbox:start_server(#{
        grpc_opts => #{
            service_protos => [tasks_pb],
            services => Routes
        },
        listen_opts => #{
            port => 50051,
            ip => {0,0,0,0}
        }
    }),
    
    io:format("gRPC Server started on port 50051~n"),
    task_grpc_server_sup:start_link().

stop(_State) ->
    ok.