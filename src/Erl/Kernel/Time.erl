-module(erl_kernel_time@foreign).

-export([seconds/0
        ,milliseconds/0
        ]).


seconds() -> fun() ->
                     float(erlang:system_time(second))
             end.


milliseconds() -> fun() ->
                          float(erlang:system_time(millisecond))
                  end.
