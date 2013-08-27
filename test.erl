-module(test).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

the_time() ->
    {reductions, X} = process_info(self(), reductions),
    X.

time(F) ->
    erlang:garbage_collect(),
    A = the_time(),
    F(),
    B = the_time(),
    B - A.

measure(Rounds, MaxSize, Gen, Eval) ->
    Results = [ measure1(MaxSize, Gen, Eval)
              || _ <- lists:seq(1, Rounds) ],
    io:format("Fitting data.~n~n"),
    graph(collate(lists:concat(Results))),
    fit().

measure1(MaxSize, Gen, Eval) ->
    Time = fun(X) -> time(fun() -> Eval(X) end) end,
    io:format("Worst case."),
    Worst = measure2(MaxSize, Gen, Time),
    io:format("~nBest case."),
    Best =
        [ {Len, -T}
        || {Len, T} <- measure2(MaxSize, Gen, fun(X) -> -Time(X) end) ],
    io:format("~n"),
    Worst ++ Best.

measure2(MaxSize, Gen, Time) ->
    measure2(MaxSize, Gen, Time, {Time([]), []}).

measure2(MaxSize, Gen, Time, {T, Xs}) ->
    io:format("."),
    case length(Xs) > MaxSize of
        true ->
            io:format(" ~w", [Xs]),
            [];
        false ->
            Cands =
              lists:concat(eqc_gen:pick(vector(1, Gen(Xs)))),
            Next =
              lists:max([{Time(Ys), Ys} || Ys <- Cands]),
            [{length(Xs), T} | measure2(MaxSize, Gen, Time, Next)]
    end.

collate(Xs) -> collate(lists:sort(Xs), 1).
collate([], _) -> [];
collate([X], N) -> [{X, N}];
collate([X,X|Xs], N) -> collate([X|Xs], N+1);
collate([X,Y|Xs], N) -> [{X, N}|collate([Y|Xs], 1)].

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
    qsort([Y || Y <- Xs, Y < X]) ++
    [X] ++
    qsort([Y || Y <- Xs, Y >= X]).

qsort2([]) ->
    [];
qsort2([X]) ->
    [X];
qsort2([X,Y]) when X > Y ->
    [Y,X];
qsort2([X,Y]) ->
    [X,Y];
qsort2(Xs) ->
    Pivot = pivot(Xs),
    qsort([X || X <- Xs, X < Pivot]) ++
    [ X || X <- Xs, X == Pivot ] ++
    qsort([X || X <- Xs, X > Pivot]).

pivot(Xs) ->
    Len = length(Xs),
    [_, Pivot, _] =
      lists:sort([lists:nth(1, Xs),
                  lists:nth((1+Len) div 2, Xs),
                  lists:nth(Len, Xs)]),
    Pivot.

msort([]) ->
    [];
msort(Xs=[_]) ->
    Xs;
msort(Xs=[X,Y]) when X =< Y ->
    Xs;
msort([X,Y]) ->
    [Y,X];
msort(Xs) ->
    {Ys, Zs} = split(Xs, [], []),
    merge(msort(Ys), msort(Zs)).

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

noise(0) ->
    ok;
noise(N) ->
    erlang:yield(),
    noise(N-1).

list_gen(Xs) ->
    ?LET(X, resize(50, int()),
         [ Ys ++ [X] ++ Zs || {Ys, Zs} <- splits(Xs) ]).

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

measure_sort() ->
    measure(10, 50, fun list_gen/1, fun lists:sort/1).

measure_isort() ->
    measure(10, 50, fun list_gen/1, fun insertion_sort/1).

measure_qsort() ->
    measure(10, 50, fun list_gen/1, fun qsort/1).

measure_qsort2() ->
    measure(10, 50, fun list_gen/1, fun qsort2/1).

measure_msort() ->
    measure(10, 50, fun list_gen/1, fun msort/1).
