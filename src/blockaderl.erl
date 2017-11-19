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
         status/3,
         containers_ip/3
         %% add_container/4
        ]).

create(Host, Port, BlockadeName, Config) ->
    http_post(Host, Port, BlockadeName, Config).

delete(Host, Port, BlockadeName) ->
    http_delete(Host, Port, BlockadeName).

status(Host, Port, BlockadeName) ->
    Result = http_get(Host, Port, BlockadeName),
    case Result of
        {ok, {{_, 200,"OK"}, _Headers, JsonString}} ->
            JsonBinary = erlang:list_to_binary(JsonString),
            Content = jsx:decode(JsonBinary, [return_maps]),
            {ok, Content};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        Error ->
            Error
    end.

containers_ip(Host, Port, BlockadeName) ->
    case status(Host, Port, BlockadeName) of
        {ok, BlockadeContainers} ->
            Containers = maps:get(<<"containers">>, BlockadeContainers, error),
            Result = maps:map(fun(_K, V) ->
                             maps:get(<<"ip_address">>, V)
                     end, Containers),
            {ok, Result};
        Error ->
            Error
    end.

% priv
blockade_path() ->
    "blockade/".

url(Host, Port, Path) ->
    "http://" ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "/" ++ blockade_path() ++ Path.

http_get(Host, Port, Path) ->
    Url = url(Host, Port, Path),
    httpc:request(Url).

http_post(Host, Port, Path, Body) ->
    Url = url(Host, Port, Path),
    Headers = [],
    ContentType = "application/json",
    Json = jsx:encode(Body),
    httpc:request(post, {Url, Headers, ContentType, Json}, [], []).

http_delete(Host, Port, Path) ->
    Url = url(Host, Port, Path),
    Headers = [],
    httpc:request(delete, {Url, Headers}, [], []).
