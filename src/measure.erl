%% The main module.
-module(measure).
-export([measure/4]).
-include("measure.hrl").
-include_lib("eqc/include/eqc.hrl").

measure(Rounds, MaxSize, Family, Axes) ->
  eqc_gen:pick(true),
  WorstResults = [ round(I, worst, MaxSize, Family, Axes) || I <- lists:seq(1, Rounds) ],
  BestResults = [ round(I, best, MaxSize, Family, Axes) || I <- lists:seq(1, Rounds) ],
  if Family#family.warmup -> ok;
     true ->
       io:format("~nFitting data.~n~n"),
       fit:fit(Axes#axes.outliers, lists:concat(WorstResults), lists:concat(BestResults))
  end.

round(I, Kind, MaxSize, Family, Axes) ->
  if Family#family.warmup -> ok;
     true -> io:format("~p. ~s case.~n", [I, kind_name(Kind)])
  end,
  Axes1 = kind_axes(Axes, Kind),
  % Insert the improved initial point in the frontier.
  InitialPoint = improve(point(Family#family.initial, Axes1), Axes1),
  Frontier = #frontier{inert = [], ert = [InitialPoint]},
  Result = run(0, Frontier, MaxSize, Family, Axes1),
  Worst = worst_case(Result),
  if Family#family.warmup -> ok;
     true -> io:format("~n~p~n~n", [Worst])
  end,
  [ kind_point(X, Kind) || X <- Result ].

kind_name(worst) -> "Worst";
kind_name(best) -> "Best".
kind_axes(Axes, worst) -> Axes;
kind_axes(Axes, best) ->
  #axes {
     size = Axes#axes.size,
     time = negate(Axes#axes.time),
     measurements = lists:map(fun negate/1, Axes#axes.measurements)
    }.

kind_point(Point, worst) -> Point;
kind_point(Point=#point{coords = [Size|Coords]}, best) ->
  Point#point{coords = [Size|[-Coord || Coord <- Coords]]}.

pretty_coord([Size, Time, _ | Rest]) ->
  [Size, Time | Rest].

coord_size([Size | _]) -> Size.
coord_time([_, Time | _]) -> Time.

worst_case(Xs) ->
  MaxSize = having_maximum(fun(#point{coords=[Size|_]}) -> Size end, Xs),
  [#point{value=Value}|_] = having_maximum(fun(#point{coords=[_,Time|_]}) -> Time end, MaxSize),
  Value.

having_maximum(F, Xs) ->
  Maximum = lists:max(lists:map(F, Xs)),
  [ X || X <- Xs, F(X) == Maximum ].

run(_Count, #frontier{inert = Inert, ert = []}, _, _, _) ->
  Inert;
run(Count, #frontier{inert = Inert, ert = [Cand|Ert]}, MaxSize, Family=#family{grow = Grow}, Axes) ->
  Frontier1 = #frontier{inert = [Cand|Inert], ert = Ert},
  Z = eqc_gen:pick(Grow(Cand#point.value)),
  Cands = [ point(Value, Axes) || Value <- lists:usort(Z), (Axes#axes.size)(Value)=< MaxSize ],
  Cands1 = [ C || C=#point{coords=[Size|_]} <- Cands ],
  % Debug information
  ErtSizes = [ coord_size(X#point.coords) || X <- Ert ], 
  io:format(" ~p (run: ~p, ert: ~p ~w, cands: ~p)           \r", [pretty_coord(Cand#point.coords), Count, length(Ert), ErtSizes, length(Cands)]),
  Frontier2 = add_cands_to_frontier(Cands1, Frontier1, Axes),
  run(Count + length(Cands), Frontier2, MaxSize, Family, Axes).

point(Value, Axes) ->
  %% OBS we use both size and -size as measurements,
  %% so that a test case only dominates test cases with the same size,
  %% and we get one test case for each size
  Funs = [Axes#axes.size, Axes#axes.time, negate(Axes#axes.size) | Axes#axes.measurements],
  #point{value = Value, coords = [ F(Value) || F <- Funs ]}.

negate(F) -> fun(X) -> -F(X) end.


% Add all candidates to the frontier.
add_cands_to_frontier(Cands, Frontier, Axes) ->
  #frontier{inert = Inert, ert = Ert} =
    lists:foldl(fun(C, F) -> add_to_frontier(C, F, Axes) end, Frontier, Cands),
  #frontier{inert = Inert, ert = lists:usort(Ert)}.


% Re-run the candidate to see if we can improve its value.
improve(Cand, Axes) ->
  Cands = [ point(Cand#point.value, Axes) || _ <- lists:seq(1, Axes#axes.repeat) ],
  lists:min([Cand|Cands]).

% Checks if X dominates Y, i.e., all of its coordinate values are
% greater than or equal to Y's coordinate values.
dominates(X, Y) ->
  lists:all(fun({A, B}) -> A >= B end, lists:zip(X#point.coords, Y#point.coords)).


% Adds the candidate to the frontier if it is not dominated by any other
% element in the frontier.
add_to_frontier(Cand, Frontier=#frontier{inert=Inert, ert=Ert}, Axes) ->
  case [ X || X <- Inert ++ Ert, dominates(X, Cand) ] of
    % If the candidate is dominated by an element, we return the frontier unchanged.
    [_|_] -> Frontier;
    [] ->
      % If the candidate is not dominated, we re-run it to try to improve its value
      % and check if it is still dominating.
      Cand1 = improve(Cand, Axes),
      case [ X || X <- Inert ++ Ert, dominates(X, Cand1) ] of
        [_|_] -> Frontier;
        [] ->
          % Remove all elements from the frontier that are dominated by the candidate.
          Inert1 = [ X || X <- Inert, not dominates(Cand1, X) ],
          Ert1 = [ X || X <- Ert, not dominates(Cand1, X) ],
          #frontier{inert=Inert1, ert=[Cand1|Ert1]}
      end
  end.


