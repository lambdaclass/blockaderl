-module(blockaderl).

-export([
         create/4,
         delete/3,
         list/2,
         get/3,

         containers_ips/3,
         containers_start/4,
         containers_stop/4,
         containers_restart/4,
         containers_kill/4,

         network_state/5,
         partitions/4,
         delete_partitions/3,
         add_containers/4,

         chaos_start/4,
         chaos_stop/3,
         chaos_status/3,
         chaos_update/4
        ]).

list(Host, Port) ->
    case http_get(Host, Port) of
        {ok, {{_, 200, _}, _, JsonString}} ->
            JsonBinary = erlang:list_to_binary(JsonString),
            Content = jsx:decode(JsonBinary, [return_maps]),
            {ok, Content};
        Error ->
            Error
    end.

create(Host, Port, BlockadeName, Config) ->
    case http_post(Host, Port, BlockadeName, Config) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

delete(Host, Port, BlockadeName) ->
    case http_delete(Host, Port, BlockadeName) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        Error ->
            Error
    end.

get(Host, Port, BlockadeName) ->
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

containers_ips(Host, Port, BlockadeName) ->
    case get(Host, Port, BlockadeName) of
        {ok, BlockadeContainers} ->
            Containers = maps:get(<<"containers">>, BlockadeContainers, error),
            Result = maps:map(fun(_K, V) ->
                             maps:get(<<"ip_address">>, V)
                     end, Containers),
            {ok, Result};
        Error ->
            Error
    end.

containers_start(Host, Port, BlockadeName, ContainersNames) ->
    containers_action(Host, Port, BlockadeName, <<"start">>, ContainersNames).

containers_stop(Host, Port, BlockadeName, ContainersNames) ->
    containers_action(Host, Port, BlockadeName, <<"stop">>, ContainersNames).

containers_restart(Host, Port, BlockadeName, ContainersNames) ->
    containers_action(Host, Port, BlockadeName, <<"restart">>, ContainersNames).

containers_kill(Host, Port, BlockadeName, ContainersNames) ->
    containers_action(Host, Port, BlockadeName, <<"kill">>, ContainersNames).

network_state(Host, Port, BlockadeName, NetworkState, ContainersNames) ->
    Path = BlockadeName ++ "/network_state",
    Content = #{network_state => NetworkState,
               container_names => ContainersNames},
    case http_post(Host, Port, Path, Content) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, 400, _}, _, _}} ->
            {error, not_valid_network_state};
        {ok, {{_, 500, _}, _, _}} ->
            {error, internal_server_error};
        Error ->
            Error
    end.

partitions(Host, Port, BlockadeName, Partitions) ->
    Path = BlockadeName ++ "/partitions",
    Content = #{partitions => Partitions},
    case http_post(Host, Port, Path, Content) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, 400, _}, _, _}} ->
            {error, partitions_must_be_a_list_of_lists};
        Error ->
            Error
    end.

delete_partitions(Host, Port, BlockadeName) ->
    Path = BlockadeName ++ "/partitions" ,
    case http_delete(Host, Port, Path) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

add_containers(Host, Port, BlockadeName, ContainersNames) ->
    Path = BlockadeName,
    Content = #{containers => ContainersNames},
    case http_put(Host, Port, Path, Content) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

chaos_start(Host, Port, BlockadeName, Config) ->
    Path = BlockadeName ++ "/chaos",
    case http_post(Host, Port, Path, Config) of
        {ok, {{_, 201, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

chaos_stop(Host, Port, BlockadeName) ->
    Path = BlockadeName ++ "/chaos",
    case http_delete(Host, Port, Path) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, 500, _}, _, "Chaos is not associated with " ++ BlockadeName}} ->
            {error, chaos_not_started};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        Error ->
            Error
    end.


chaos_status(Host, Port, BlockadeName) ->
    Path = BlockadeName ++ "/chaos",
    Result = http_get(Host, Port, Path),
    case Result of
        {ok, {{_, 200, "OK"}, _Headers, JsonString}} ->
            JsonBinary = erlang:list_to_binary(JsonString),
            Content = jsx:decode(JsonBinary, [return_maps]),
            {ok, Content};
       {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        Error ->
            Error
    end.

chaos_update(Host, Port, BlockadeName, Config) ->
    Path = BlockadeName ++ "/chaos",
    case http_put(Host, Port, Path, Config) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

%% priv
containers_action(Host, Port, BlockadeName, Action, ContainersNames) ->
    Path = BlockadeName ++ "/action",
    Content = #{command => Action,
                container_names => ContainersNames},
    case http_post(Host, Port, Path, Content) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        Error ->
            Error
    end.

% helpers
blockade_path() ->
    "blockade".

url(Host, Port) when is_binary(Host) ->
    HostString = erlang:binary_to_list(Host),
    url(HostString, Port);
url(Host, Port) ->
    "http://" ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "/" ++ blockade_path().

url(Host, Port, Path) ->
    url(Host, Port) ++ "/" ++ Path.

http_get(Host, Port) ->
    Url = url(Host, Port),
    httpc:request(Url).

http_get(Host, Port, Path) ->
    Url = url(Host, Port, Path),
    httpc:request(Url).

http_post(Host, Port, Path, Body) ->
    Url = url(Host, Port, Path),
    Headers = [],
    ContentType = "application/json",
    Json = jsx:encode(Body),
    httpc:request(post, {Url, Headers, ContentType, Json}, [], []).

http_put(Host, Port, Path, Body) ->
    Url = url(Host, Port, Path),
    Headers = [],
    ContentType = "application/json",
    Json = jsx:encode(Body),
    httpc:request(put, {Url, Headers, ContentType, Json}, [], []).

http_delete(Host, Port, Path) ->
    Url = url(Host, Port, Path),
    Headers = [],
    httpc:request(delete, {Url, Headers}, [], []).
