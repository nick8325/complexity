%% The main module.
-module(measure).
-export([measure/4]).
-include("measure.hrl").
-include_lib("eqc/include/eqc.hrl").

measure(Rounds, MaxSize, Family, Axes) ->
  eqc_gen:pick(true),
  Results =
    [ round(I, MaxSize, Family, Axes)
    || I <- lists:seq(1, Rounds) ],
  io:format("Fitting data.~n~n"),
  Results1 =
    [{N, X} || #point{coords=[N,X|_]} <- lists:concat(Results) ],
  fit:fit(Results1, Results1).

round(I, MaxSize, Family, Axes) ->
  io:format("~p.", [I]),
  Frontier = #frontier{inert = [], ert = [point(Family#family.initial, Axes)]},
  Result = run(Frontier, MaxSize, Family, Axes),
  Worst = worst_case(Result),
  io:format("~n~p~n~n", [Worst]),
  Result.

worst_case(Xs) ->
  MaxSize = having_maximum(fun(#point{coords=[Size|_]}) -> Size end, Xs),
  [#point{value=Value}|_] = having_maximum(fun(#point{coords=[_,Time|_]}) -> Time end, MaxSize),
  Value.

having_maximum(F, Xs) ->
  Maximum = lists:max(lists:map(F, Xs)),
  [ X || X <- Xs, F(X) == Maximum ].

run(#frontier{inert = Inert, ert = []}, _, _, _) ->
  Inert;
run(#frontier{inert = Inert, ert = [Cand|Ert]}, MaxSize, Family=#family{grow = Grow}, Axes) ->
  Frontier1 = #frontier{inert = [Cand|Inert], ert = Ert},
  Z = eqc_gen:pick(Grow(Cand#point.value)),
  Cands = [ point(Value, Axes) || Value <- Z ],
  Cands1 = [ C || C=#point{coords=[Size|_]} <- Cands, Size =< MaxSize ],
  io:format("."),
  run(add_cands_to_frontier(Cands1, Frontier1), MaxSize, Family, Axes).

point(Value, Axes) ->
  %% OBS we use both size and -size to the measurements,
  %% so that a test case only dominates test cases with the same size,
  %% and we get one test case for each size
  Funs = [Axes#axes.size, Axes#axes.time, negate(Axes#axes.size)|Axes#axes.measurements],
  #point{value = Value, coords = [ F(Value) || F <- Funs ]}.

negate(F) -> fun(X) -> -F(X) end.

add_cands_to_frontier(Cands, Frontier) ->
  lists:foldl(fun add_to_frontier/2, Frontier, Cands).

add_to_frontier(Cand, Frontier=#frontier{inert=Inert, ert=Ert}) ->
  case [ X || X <- Inert ++ Ert, dominates(X, Cand) ] of
    [_|_] -> Frontier;
    [] ->
      Inert1 = [ X || X <- Inert, not dominates(Cand, X) ],
      Ert1 = [ X || X <- Ert, not dominates(Cand, X) ],
      #frontier{inert=Inert1, ert=[Cand|Ert1]}
  end.

dominates(X, Y) ->
  lists:all(fun({A, B}) -> A >= B end, lists:zip(X#point.coords, Y#point.coords)).
