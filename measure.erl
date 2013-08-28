%% The main module.
-module(measure).
-export([measure/6]).
-include_lib("eqc/include/eqc.hrl").

measure(Rounds, Points, Size, X0, Gen, Eval) ->
    eqc_gen:pick(true),
    Time = fun(X) -> timing:time(Eval, [X]) end,
    Results =
      [ round(worst, Points, Size, X0, Gen, Time) ++
        round(best, Points, Size, X0, Gen, Time)
      || _ <- lists:seq(1, Rounds) ],
    io:format("Fitting data.~n~n"),
    fit:fit(lists:concat(Results)).

type_name(worst) -> "Worst";
type_name(best) -> "Best".
type_multiplier(worst) -> 1;
type_multiplier(best) -> -1.

round(Type, Points, Size, X0, Gen, Time) ->
    io:format("~s case.", [type_name(Type)]),
    DirectedTime =
        fun(X) -> type_multiplier(Type) * Time(X) end,
    Result = run(Points, Size, X0, Gen, DirectedTime),
    io:format("~n"),
    [ {Len, type_multiplier(Type) * T}
    || {Len, T} <- Result ].

run(Points, Size, X0, Gen, Time) ->
    loop(Points, Size, Gen, Time, {Time(X0), X0}).

loop(0, _Size, _Gen, _Time, {_TimeX, X}) ->
    io:format(" ~w", [X]),
    [];
loop(Points, Size, Gen, Time, {TimeX, X}) ->
    io:format("."),
    Cands = eqc_gen:pick(Gen(X)),
    Next = lists:max([{Time(Y), Y} || Y <- Cands ]),
    [{Size(X), TimeX} | loop(Points-1, Size, Gen, Time, Next)].
