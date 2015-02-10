% Sorting examples.
-module(example_sorting).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").

%% A variety of sorting algorithms.
naive_qsort([]) ->
    [];
naive_qsort([X|Xs]) ->
    naive_qsort([Y || Y <- Xs, Y < X]) ++
    [X] ++
    naive_qsort([Y || Y <- Xs, Y >= X]).

median_of_three_qsort([]) ->
    [];
median_of_three_qsort([X]) ->
    [X];
median_of_three_qsort([X,Y]) when X > Y ->
    [Y,X];
median_of_three_qsort([X,Y]) ->
    [X,Y];
median_of_three_qsort(Xs) ->
    Pivot = pivot(Xs),
    median_of_three_qsort([X || X <- Xs, X < Pivot]) ++
    [ X || X <- Xs, X == Pivot ] ++
    median_of_three_qsort([X || X <- Xs, X > Pivot]).

pivot(Xs) ->
    Len = length(Xs),
    [_, Pivot, _] =
      lists:sort([lists:nth(1, Xs),
                  lists:nth((1+Len) div 2, Xs),
                  lists:nth(Len, Xs)]),
    Pivot.

%% An incremental list generator.
list_gen(Xs) ->
    ?LET(Ys, vector(5, resize(100, int())),
    ?LET(Zs, vector(5, elements(non_empty(0, Xs))),
      return(insert_anywhere(Ys ++ Zs, Xs)))).

non_empty(X, []) ->
    [X];
non_empty(_X, Xs) ->
    Xs.

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

insert_anywhere(Xs, Ys) ->
    [ As ++ [X] ++ Bs || X <- Xs, {As, Bs} <- splits(Ys) ].

insertion_sort([]) ->
    [];
insertion_sort([X|Xs]) ->
    ordsets:add_element(X, insertion_sort(Xs)).

merge_sort([]) ->
    [];
merge_sort(Xs=[_]) ->
    Xs;
merge_sort(Xs=[X,Y]) when X =< Y ->
    Xs;
merge_sort([X,Y]) ->
    [Y,X];
merge_sort(Xs) ->
    {Ys, Zs} = split(Xs, [], []),
    merge(merge_sort(Ys), merge_sort(Zs)).

split([], Ys, Zs) ->
    {Ys, Zs};
split([X|Xs], Ys, Zs) ->
    split(Xs, [X|Zs], Ys).

merge(Xs, []) ->
    Xs;
merge([], Ys) ->
    Ys;
merge([X|Xs], Ys=[Y|_]) when X =< Y ->
    [X|merge(Xs, Ys)];
merge(Xs, [Y|Ys]) ->
    [Y|merge(Xs, Ys)].

measure_sorting_algorithm(Sort) ->
  measure(10, 50,
          #family{initial = [], grow = fun list_gen/1},
          #axes{size = fun length/1,
                time = time1(Sort)}).

measure_sort() ->
    measure_sorting_algorithm(fun lists:sort/1).

measure_insertion_sort() ->
    measure_sorting_algorithm(fun insertion_sort/1).

measure_naive_qsort() ->
    measure_sorting_algorithm(fun naive_qsort/1).

measure_median_of_three_qsort() ->
    measure_sorting_algorithm(fun median_of_three_qsort/1).

measure_msort() ->
    measure_sorting_algorithm(fun merge_sort/1).
