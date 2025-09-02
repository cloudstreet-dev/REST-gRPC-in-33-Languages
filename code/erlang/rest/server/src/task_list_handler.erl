-module(task_list_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    handle_request(Method, Req0, State).

handle_request(<<"GET">>, Req0, State) ->
    %% Parse query parameters
    Qs = cowboy_req:parse_qs(Req0),
    Filters = parse_filters(Qs),
    
    case task_store:list_tasks(Filters) of
        {ok, Tasks} ->
            Response = #{
                <<"tasks">> => Tasks,
                <<"total_count">> => length(Tasks)
            },
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(Response), Req0);
        {error, _Reason} ->
            Req = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{<<"error">> => <<"Internal server error">>}), Req0)
    end,
    {ok, Req, State};

handle_request(<<"POST">>, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try jiffy:decode(Body, [return_maps]) of
        TaskData ->
            case maps:get(<<"title">>, TaskData, undefined) of
                undefined ->
                    Req = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jiffy:encode(#{<<"error">> => <<"Missing required field: title">>}), Req1);
                _Title ->
                    case task_store:create_task(TaskData) of
                        {ok, Task} ->
                            Req = cowboy_req:reply(201, #{
                                <<"content-type">> => <<"application/json">>
                            }, jiffy:encode(Task), Req1);
                        {error, Reason} ->
                            Req = cowboy_req:reply(500, #{
                                <<"content-type">> => <<"application/json">>
                            }, jiffy:encode(#{<<"error">> => list_to_binary(io_lib:format("~p", [Reason]))}), Req1)
                    end
            end
    catch
        _:_ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{<<"error">> => <<"Invalid JSON">>}), Req1)
    end,
    {ok, Req, State};

handle_request(_, Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
    {ok, Req, State}.

parse_filters(Qs) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        case Key of
            <<"status">> -> maps:put(<<"status">>, Value, Acc);
            <<"assigned_to">> -> maps:put(<<"assigned_to">>, Value, Acc);
            _ -> Acc
        end
    end, #{}, Qs).