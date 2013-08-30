%% Time how long an operation takes.
-module(timing).
-export([time_and_result/1, time/1, time/2, time1/1]).

the_time() ->
    {reductions, X} = process_info(self(), reductions),
    X.

time_and_result(Fun) ->
    erlang:garbage_collect(),
    A = the_time(),
    X = Fun(),
    B = the_time(),
    {B - A, X}.

time(Fun) ->
    {Time, _} = time_and_result(Fun),
    Time.

time(Fun, Args) ->
    time(fun() -> apply(Fun, Args) end).

time1(Fun) ->
    fun(X) -> time(Fun, [X]) end.
