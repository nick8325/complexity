-module(test).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

%the_time() ->
%    Million = 1000000,
%    {X,Y,Z} = now(),
%    X*Million*Million + Y*Million + Z.

the_time() ->
    {reductions, X} = process_info(self(), reductions),
    X.

time(F) ->
    erlang:garbage_collect(),
    A = the_time(),
    F(),
    B = the_time(),
    B - A.

measure(Tests, MaxSize, Gen, Size, Eval) ->
    {Data, _} = eqc_gen:gen(function1(Gen), MaxSize, []),
    io:format("Running ~p tests", [Tests]),
    Results = measure1(Tests, Data, Size, Eval, []),
    io:format(" done! Fitting data.~n~n"),
    graph(collate(Results)),
    fit().

measure1(0, _Data, _Size, _Eval, Results) ->
    Results;
measure1(N, Data, Size, Eval, Results) ->
    io:format("."),
    X = Data(N),
    Result = {Size(X), time(fun() -> Eval(X) end)},
    measure1(N-1, Data, Size, Eval, [Result|Results]).

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
    qsort([Y || Y <- Xs, Y =< X]) ++
    [X] ++
    qsort([Y || Y <- Xs, Y >= X]).

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

measure_msort() ->
    measure(1000, 100, list_gen(),
            fun length/1,
            fun msort/1).

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

measure_noise() ->
    measure(1000, 10000, nat(), fun(X) -> X end, fun noise/1).
