-module(task_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Id = cowboy_req:binding(id, Req0),
    handle_request(Method, Id, Req0, State).

handle_request(<<"GET">>, Id, Req0, State) ->
    case task_store:get_task(Id) of
        {ok, Task} ->
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(Task), Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{<<"error">> => <<"Task not found">>}), Req0)
    end,
    {ok, Req, State};

handle_request(<<"PUT">>, Id, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    try jiffy:decode(Body, [return_maps]) of
        Updates ->
            case task_store:update_task(Id, Updates) of
                {ok, Task} ->
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, jiffy:encode(Task), Req1);
                {error, not_found} ->
                    Req = cowboy_req:reply(404, #{
                        <<"content-type">> => <<"application/json">>
                    }, jiffy:encode(#{<<"error">> => <<"Task not found">>}), Req1)
            end
    catch
        _:_ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{<<"error">> => <<"Invalid JSON">>}), Req1)
    end,
    {ok, Req, State};

handle_request(<<"DELETE">>, Id, Req0, State) ->
    case task_store:delete_task(Id) of
        ok ->
            Req = cowboy_req:reply(204, Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jiffy:encode(#{<<"error">> => <<"Task not found">>}), Req0)
    end,
    {ok, Req, State};

handle_request(_, _Id, Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(#{<<"error">> => <<"Method not allowed">>}), Req0),
    {ok, Req, State}.