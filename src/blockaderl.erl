-module(blockaderl).

-export([
         create/4,
         delete/3,
         %% containers_start/4,
         %% containers_stop/4,
         %% containers_restart/4,
         %% containers_kill/4,
         %% update_network_state/4,
         %% partition/4,
         %% delete_partitions/4,
         %% list/4,
         %% status/4,
         %% add_container/4
         get/3
        ]).

create(Host, Port, Name, Config) ->
    Path = "blockade/" ++ Name,
    post(Host, Port, Path, Config).

delete(Host, Port, Name) ->
    Path = "blockade/" ++ Name,
    http_delete(Host, Port, Path).

% priv
get(Host, _Port, Path) ->
    Url = Host + Path,
    httpc:request(Url).

post(Host, Port, Path, Body) ->
    Url = "http://" ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "/" ++ Path,
    Headers = [],
    ContentType = "application/json",
    Json = jsx:encode(Body),
    httpc:request(post, {Url, Headers, ContentType, Json}, [], []).

http_delete(Host, Port, Path) ->
    Url = "http://" ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "/" ++ Path,
    Headers = [],
    httpc:request(delete, {Url, Headers}, [], []).


%% Body = #{
%%   containers => #{
%%     reconnections => #{
%%       image => <<"erlang:20">>,
%%       volumes => #{
%%         <<".">> => <<"/opt/reconnections">>
%%       },
%%       command => <<"sleep infinity">>
%%     },
%%     postgres => #{
%%       image => <<"postgres">>,
%%       ports => #{
%%         <<"5432">> => 5432
%%       },
%%       environment => #{
%%         <<"POSTGRES_PASSWORD">> => <<"example">>
%%       }
%%     },
%%     redis => #{
%%       image => <<"redis:alpine">>,
%%       <<"ports">> => #{
%%         <<"6379">> => 6379
%%       }
%%     }
%%   }
%%  },
%% blockaderl:create("localhost", 5000,  "a", Body).
%% blockaderl:delete("localhost", 5000, "a").
