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

measure(Rounds, Points, Size, X0, Gen, Eval) ->
    eqc_gen:pick(true),
    Results = [ measure1(Points, Size, X0, Gen, Eval)
              || _ <- lists:seq(1, Rounds) ],
    io:format("Fitting data.~n~n"),
    graph(collate(lists:concat(Results))),
    fit().

measure1(Points, Size, X0, Gen, Eval) ->
    Time = fun(X) -> time(fun() -> Eval(X) end) end,
    io:format("Worst case."),
    Worst = measure2(Points, Size, X0, Gen, Time),
    io:format("~nBest case."),
    Best =
        [ {Len, -T}
        || {Len, T} <- measure2(Points, Size, X0, Gen, fun(X) -> -Time(X) end) ],
    io:format("~n"),
    Worst ++ Best.

measure2(Points, Size, X0, Gen, Time) ->
    measure3(Points, Size, Gen, Time, {Time(X0), X0}).

measure3(0, _Size, _Gen, _Time, {_TimeX, X}) ->
    io:format(" ~w", [X]),
    [];
measure3(Points, Size, Gen, Time, {TimeX, X}) ->
    io:format("."),
    Cands = eqc_gen:pick(Gen(X)),
    Next = lists:max([{Time(Y), Y} || Y <- Cands ]),
    [{Size(X), TimeX} | measure3(Points-1, Size, Gen, Time, Next)].

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
         return([ Ys ++ [X] ++ Zs || {Ys, Zs} <- splits(Xs) ])).

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

measure_sort() ->
    measure(10, 50, fun length/1, [], fun list_gen/1, fun lists:sort/1).

measure_isort() ->
    measure(10, 50, fun length/1, [], fun list_gen/1, fun insertion_sort/1).

measure_qsort() ->
    measure(10, 50, fun length/1, [], fun list_gen/1, fun qsort/1).

measure_qsort2() ->
    measure(10, 50, fun length/1, [], fun list_gen/1, fun qsort2/1).

measure_msort() ->
    measure(10, 50, fun length/1, [], fun list_gen/1, fun msort/1).

measure_lookup_gbsets() ->
    measure(10, 500,
            fun({_, T}) -> gb_sets:size(T) end,
            {0, gb_sets:new()},
            fun gbsets_gen/1,
            fun({X, T}) -> gb_sets:is_element(X, T) end).

gbsets_gen({X, T}) ->
  ?LET(Y, resize(1000, int()),
       return(
       [ {X, gb_sets:add(Y, T)},
         {Y, T},
         {Y, gb_sets:add(Y, T)} ])).

reverse(Xs) ->
    reverse(Xs, []).
reverse([], Ys) ->
    Ys;
reverse([X|Xs], Ys) ->
    reverse(Xs, [X|Ys]).

in1(X, {Xs, Ys}) ->
    {[X|Xs], Ys}.
in2(Y, {Xs, Ys}) ->
    {Xs, [Y|Ys]}.
out1({[], []}) ->
    error(empty_queue);
out1({[], Ys}) ->
    [X|Xs] = reverse(Ys),
    {X, {Xs, []}};
out1({[X|Xs], Ys}) ->
    {X, {Xs, Ys}}.
out2({[], []}) ->
    error(empty_queue);
out2({Xs, []}) ->
    [Y|Ys] = reverse(Xs),
    {Y, {[], Ys}};
out2({Xs, [Y|Ys]}) ->
    {Y, {Xs, Ys}}.
len({Xs, Ys}) ->
    length(Xs) + length(Ys).
empty({[], []}) ->
    true;
empty(_) ->
    false.

cmd(out1, Q) ->
    element(2, out1(Q));
cmd(out2, Q) ->
    element(2, out2(Q));
cmd(in1, Q) ->
    in1(a, Q);
cmd(in2, Q) ->
    in2(a, Q).

cmds(Q) ->
    [ out1 || not empty(Q) ] ++
    [ out2 || not empty(Q) ] ++
    [ in1, in2 ].

queue_gen({_Cmd, Q}) ->
    return(
    [ {Cmd, Q1}
    || Cmd1 <- cmds(Q),
       Q1 <- [cmd(Cmd1, Q)],
       Cmd <- cmds(Q1) ]).

measure_queue() ->
    measure(10, 50,
            fun({_Cmd, Q}) -> len(Q) end,
            {in1, {[],[]}},
            fun queue_gen/1,
            fun({Cmd, Q}) -> cmd(Cmd, Q) end).
