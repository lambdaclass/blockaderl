-module(basic_SUITE).

-export([all/0]).
-export([basic/1]).

all() ->
    [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

basic(_Config) ->
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
    ok = blockaderl:create("localhost", 5000, "a", BlockadeContainers),
    ok = blockaderl:delete("localhost", 5000, "a"),
    ok.
