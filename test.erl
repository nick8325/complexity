-module(test).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

time(F) ->
    {reductions, A} = process_info(self(), reductions),
    F(),
    {reductions, B} = process_info(self(), reductions),
    B - A.

measure(Tests, MaxSize, Gen, Size, Eval) ->
    Self = self(),
    Print = fun(X) -> Self ! {print, X} end,
    eqc:quickcheck(eqc:numtests(Tests, eqc_gen:resize(MaxSize,
    ?FORALL(X, Gen,
    begin
      erlang:garbage_collect(),
      collect(Print, {Size(X), time(fun() -> Eval(X) end)}, true)
    end)))),
    receive {print, Res} -> graph(Res), fit() end.

graph(Points) ->
    file:write_file("data",
      [ io_lib:format("~p ~p ~p~n", [X, Y, K]) || {{X, Y}, K} <- Points ]).

fit() ->
    io:put_chars(os:cmd("ghc --make -O Fit && ./Fit && gnuplot -persist gnuplot")).

insertion_sort([]) ->
    [];
insertion_sort([X|Xs]) ->
    ordsets:add_element(X, insertion_sort(Xs)).

qsort([]) ->
    [];
qsort([X|Xs]) ->
    qsort([Y || Y <- Xs, Y =< X]) ++
    [X] ++
    qsort([Y || Y <- Xs, Y >= X]).

list_gen() ->
    frequency([{50, list(int())},
               {1, ?LET(Xs, list(int()), lists:sort(Xs))},
               {1, ?LET(Xs, list(int()), lists:reverse(lists:sort(Xs)))}]).

measure_sort() ->
    measure(1000, 1000, list_gen(),
            fun length/1,
            fun lists:sort/1).

measure_isort() ->
    measure(1000, 100, list_gen(),
            fun length/1,
            fun insertion_sort/1).

measure_qsort() ->
    measure(1000, 100, list_gen(),
            fun length/1,
            fun qsort/1).

measure_lookup_gbsets() ->
    measure(1000, 1000,
            ?LET(Xs, list_gen(),
                 {int(), gb_sets:from_list(Xs)}),
            fun({_, T}) -> gb_sets:size(T) end,
            fun({X, T}) -> gb_sets:is_element(X, T) end).

measure_queue() ->
    measure(1000, 1000, list_gen(),
            fun length/1,
            fun(Xs) ->
                Q = lists:foldl(fun queue:in/2, queue:new(), Xs),
                lists:foldl(fun(_, Q2) -> element(2, queue:out(Q2)) end, Q, Xs)
            end).
