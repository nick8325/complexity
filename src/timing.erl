%% Time how long an operation takes.
-module(timing).
-export([time/1, time/2, time1/1]).

the_time() ->
    {reductions, X} = process_info(self(), reductions),
    X.

time(Fun) ->
    erlang:garbage_collect(),
    A = the_time(),
    Fun(),
    B = the_time(),
    B - A.

time(Fun, Args) ->
    time(fun() -> apply(Fun, Args) end).

time1(Fun) ->
    fun(X) -> time(Fun, [X]) end.
