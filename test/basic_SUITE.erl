-module(basic_SUITE).

-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

-export([basic/1]).

all() ->
    [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

init_per_suite(_) ->
    application:ensure_all_started(blockaderl),
    BlockadeContainers = #{
      containers => #{
        reconnections => #{
          image => <<"erlang:20">>,
          volumes => #{
            <<".">> => <<"/opt/reconnections">>
           },
          command => <<"sleep infinity">>
         },
        postgres => #{
          image => <<"postgres">>,
          ports => #{
            <<"5432">> => 5432
           },
          environment => #{
            <<"POSTGRES_PASSWORD">> => <<"example">>
           }
         },
        redis => #{
          image => <<"redis:alpine">>,
          <<"ports">> => #{
              <<"6379">> => 6379
             }
         }
       }
     },
    Config = [{containers, BlockadeContainers},
              {host, "localhost"},
              {port, 5000},
              {name, <<"test">>}],
    Config.

end_per_suite(Config) ->
  Config.

%% tests
basic(Config) ->
    Host = proplists:get_value(host, Config),
    Port =  proplists:get_value(port, Config),
    Name =  proplists:get_value(name, Config),
    Containers =  proplists:get_value(containers, Config),

    ok = blockaderl:create(Host, Port, Name, Containers),
    {ok, Blockades} = blockaderl:list(Host, Port),
    #{<<"blockades">> := [Name]} = Blockades,

    {ok, BlockadeStatus} = blockaderl:get(Host, Port, Name),
    #{<<"containers">> := BlockadeContainers} = BlockadeStatus,
    [<<"postgres">>, <<"reconnections">>, <<"redis">>] = maps:keys(BlockadeContainers),

    ok = blockaderl:delete(Host, Port, "test"),
    {ok, BlockadesPostDelete} = blockaderl:list(Host, Port),
    #{<<"blockades">> := []} = BlockadesPostDelete,
    ok.
