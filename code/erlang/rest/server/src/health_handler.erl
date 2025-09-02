-module(health_handler).

-export([init/2]).

init(Req0, State) ->
    Response = #{
        <<"status">> => <<"healthy">>,
        <<"service">> => <<"task-api">>,
        <<"version">> => <<"1.0.0">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jiffy:encode(Response), Req0),
    
    {ok, Req, State}.