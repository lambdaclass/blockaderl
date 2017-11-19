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
         network_state/5
         %% add_container/4,
         %% partition/4,
         %% delete_partitions/4,
        ]).

list(Host, Port) ->
    http_get(Host, Port).

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

%% priv
containers_action(Host, Port, BlockadeName, Action, ContainersNames) ->
    Path = BlockadeName ++ "/action",
    Content = #{command => Action,
                container_names => ContainersNames},
    case http_post(Host, Port, Path, Content) of
        {ok, {{_, 204, _}, _, _}} ->
            ok;
        Error ->
            Error
    end.

% helpers
blockade_path() ->
    "blockade".

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

http_delete(Host, Port, Path) ->
    Url = url(Host, Port, Path),
    Headers = [],
    httpc:request(delete, {Url, Headers}, [], []).
