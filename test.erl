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
    measure(Tests, MaxSize, Gen, Size, Eval, fun(_) -> [] end).

measure(Tests, MaxSize, Gen, Size, Eval, Opt) ->
    Data = eqc_gen:pick(
        function1(
        ?LET(N, choose(0, MaxSize),
        resize(N, Gen)))),
    io:format("Running ~p tests", [Tests]),
    Results = measure1(Tests, Data, Size, Eval, Opt, []),
    io:format(" done! Fitting data.~n~n"),
    graph(collate(Results)),
    fit().

measure1(0, _Data, _Size, _Eval, _Opt, Results) ->
    Results;
measure1(N, Data, Size, Eval, Opt, Results) ->
    io:format("."),
    X = Data(N),
    Result1 = {Size(X), anneal(fun(Y) -> time(fun() -> Eval(Y) end) end, X, Opt)},
    Result2 = {Size(X), -anneal(fun(Y) -> -time(fun() -> Eval(Y) end) end, X, Opt)},
    measure1(N-1, Data, Size, Eval, Opt, [Result1, Result2|Results]).

probability() ->
    K = 65536,
    N = eqc_gen:pick(choose(0, K-1)),
    N / K.

anneal(Time, X, Opt) ->
    TimeX = Time(X),
    Temp = TimeX * TimeX,
    anneal(Temp, Time, TimeX, X, Opt).

anneal(Temp, Time, TimeX, X, Opt) ->
    %io:format("~p ~p ~p~n", [Temp, TimeX, X]),
    {TimeY, Y} = anneal1(Temp, Time, TimeX, X, Opt),
    if
        Temp < 1 andalso TimeX == TimeY ->
            TimeX;
        true ->
            anneal(Temp * 0.8, Time, TimeY, Y, Opt)
    end.

sample(0, _) -> [];
sample(N, G={M, Fun}) ->
  ?LET(I, choose(0, M-1),
       case Fun(I) of
         [] -> sample(N, G);
         [X] -> [X|sample(N-1, G)];
         _ -> erlang:error({nondeterministic_generator, Fun(I)})
       end).

take(N, Xs) ->
    {Ys, _Zs} = lists:split(N, Xs),
    Ys.

anneal1(Temp, Time, TimeX, X, Opt) ->
    Xs = eqc_gen:pick(sample(abs(TimeX), Opt(X))),
    anneal2(Xs, Temp, Time, TimeX, X).

anneal2([], _Temp, _Time, TimeX, X) ->
    {TimeX, X};
anneal2([Y|Ys], Temp, Time, TimeX, X) ->
    TimeY = Time(Y),
    % hack: don't compute Diff if TimeX =< TimeY
    Diff = math:exp((TimeY - max(TimeX, TimeY)) / Temp),
    Prob = probability(),
    if
        TimeX =< TimeY ->
            % io:format("improved ~p to ~p~n", [X, Y]),
            anneal2(Ys, Temp, Time, TimeY, Y);
        Diff > Prob ->
            % io:format("decayed ~p to ~p with diff ~p (~p -> ~p) at temp ~p~n", [X, Y, Diff, TimeX, TimeY, Temp]),
            anneal2(Ys, Temp, Time, TimeY, Y);
        true ->
            anneal2(Ys, Temp, Time, TimeX, X)
    end.

opt(Time, X, Opt) ->
    io:format("opt ~p (~p)~n", [X, Time(X)]),
    opt(Time, Time(X), X, Opt).

opt(Time, Time0, X, Opt) ->
    Times = lists:sort([{Time(Y), Y} || Y <- enumerate(Opt(X)) ]),
    case Times of
        [{Time1, Y}|_] when Time1 < Time0 ->
            %io:format("mutated ~p (~p) to ~p (~p)~n",
            %    [X, Time0, Y, -Time1]),
            opt(Time, Time1, Y, Opt);
        _ ->
            io:format("stopped at ~p (~p)~n", [X, Time0]),
            Time0
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
    ?SIZED(N, vector(N, int())).

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

enumerate({N, Fun}) ->
    lists:concat([ Fun(I) || I <- lists:seq(0, N-1) ]).

const(X) ->
    {1, fun(_) -> [X] end}.

either({M, Fun1}, {N, Fun2}) ->
    {M+N,
     fun(I) when I < M -> Fun1(I);
        (I) -> Fun2(I - M)
     end}.

any([X]) ->
    X;
any([X|Xs]) ->
    either(X, any(Xs)).

pair(M, N, Fun) ->
    {M * N,
     fun(I) -> Fun(I rem M, I div M) end}.

ordered_pair(N, Fun) ->
    pair(N, N,
         fun(I, J) -> Fun(min(I, J), max(I, J)) end).

partition(N, Fun) ->
    ordered_pair(N,
                 fun(I, J) -> Fun(I, J-I) end).

card(N, {M, Fun}) ->
  {M*N,
   fun(I) ->
     at(I rem N, Fun(I div N))
   end}.

at(_, []) -> [];
at(0, [X|_]) -> [X];
at(N, [_|Xs]) -> at(N-1, Xs).

mutate_list(Xs) ->
    Len = length(Xs),
    any(
      [ partition(Len,
        fun(I, J) ->
          [ As ++ [Y] ++ Bs ++ [X] ++ Cs
          || {As, [X|Ys]} <- [lists:split(I, Xs)],
             {Bs, [Y|Cs]} <- [lists:split(J, Ys)] ]
        end),

        card(3,
        partition(Len+1,
        fun(I, J) ->
          {As, Ys} = lists:split(I, Xs),
          {Bs, Cs} = lists:split(J, Ys),
          [ Bs ++ As ++ Cs,
            As ++ Cs ++ Bs,
            Cs ++ Bs ++ As ]
        end))]).

randomly_permute(Xs) ->
    ?LET(I, choose(0, length(Xs)),
    ?LET(J, choose(I, length(Xs)),
    begin
        {As, Ys} = lists:split(I, Xs),
        {Bs, Cs} = lists:split(J-I, Ys),
        ?LET(Bs1, shuffle(Bs),
        As ++ Bs1 ++ Cs)
    end)).

mutate_gbset({X,T}) ->
    [ {Y, T} || Y <- gb_sets:to_list(T) ] ++
    [ {X, gb_sets:insert(Y, gb_sets:delete(Y, T))} || Y <- gb_sets:to_list(T) ].

measure_sort() ->
    measure(20, 50, list_gen(),
            fun length/1,
            fun lists:sort/1,
            fun mutate_list/1).

measure_isort() ->
    measure(20, 50, list_gen(),
            fun length/1,
            fun insertion_sort/1,
            fun mutate_list/1).

measure_qsort() ->
    measure(20, 50, list_gen(),
            fun length/1,
            fun qsort/1,
            fun mutate_list/1).

measure_msort() ->
    measure(20, 100, list_gen(),
            fun length/1,
            fun msort/1,
            fun mutate_list/1).

measure_lookup_gbsets() ->
    measure(20, 1000,
            ?LET(Xs, list_gen(),
                 {int(), gb_sets:from_list(Xs)}),
            fun({_, T}) -> gb_sets:size(T) end,
            fun({X, T}) -> gb_sets:is_element(X, T) end,
            fun mutate_gbset/1).

measure_queue() ->
    measure(200, 1000, list_gen(),
            fun length/1,
            fun(Xs) ->
                Q = lists:foldl(fun queue:in/2, queue:new(), Xs),
                lists:foldl(fun(_, Q2) -> element(2, queue:out(Q2)) end, Q, Xs)
            end).

measure_noise() ->
    measure(1000, 10000, nat(), fun(X) -> X end, fun noise/1).
