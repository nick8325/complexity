%% The main module.
-module(measure).
-export([measure/6]).
-include_lib("eqc/include/eqc.hrl").

-record(point, {size, time, value}).
-record(candidate, {size, time, value, depth}).

measure(Rounds, Points, Size, X0, Gen, Eval) ->
    eqc_gen:pick(true),
    Results =
      [ {round(worst, Points, Size, X0, Gen, Time),
         round(best, Points, Size, X0, Gen, Time)}
      || _ <- lists:seq(1, Rounds) ],
    io:format("Fitting data.~n~n"),
    {Worsts, Bests} = lists:unzip(Results),
    fit:fit(lists:concat(Worsts), lists:concat(Bests)).

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

insert(Point = #point{size = Size, time = Time}, Tree) ->
    case gb_trees:lookup(Size, Tree) of
        {value, Existing} when Existing#point.time >= Time ->
            not_optimal;
        _ ->
            {optimal, gb_trees:enter(Size, Point, Tree)}
    end.

insert_cand(Cand = #candidate{size = Size, time = Time}, Queue) ->
    priority_queue:insert({Size, -Time, Cand}, Queue).

insert_cands([], H) ->
    H;
insert_cands([X|Xs], H) ->
    insert_cands(Xs, insert_cand(X, H)).

remove_cand(Queue) ->
    case priority_queue:remove_min(Queue) of
        empty -> empty;
        {found, {_, _, Cand}, Queue1} ->
            {found, Cand, Queue1}
    end.

run(Points, Size, X0, Gen, Time) ->
    Cand = #candidate{value = X0, depth = Points, time = Time(X0), size = Size(X0)},
    loop(Size, Gen, Time, gb_trees:empty(),
         insert_cand(Cand, priority_queue:new())).

loop(Size, Gen, Time, Best, Queue) ->
    case remove_cand(Queue) of
        empty ->
            finished(Best);
        {found, #candidate{size = SizeX, time = TimeX, value = X, depth = Depth}, Queue1} ->
            Point = #point{size = SizeX,
                           time = TimeX,
                           value = X},
            case insert(Point, Best) of
                not_optimal ->
                    loop(Size, Gen, Time, Best, Queue1);
                {optimal, Best1} ->
                    io:format("."),
                    Cands =
                      [ #candidate{value = Y, depth = Depth-1, size = Size(Y), time = Time(Y)}
                      || Depth > 0,
                         Y <- eqc_gen:pick(Gen(X)) ],
                    loop(Size, Gen, Time, Best1,
                         insert_cands(Cands, Queue1))
            end
    end.

finished(Best) ->
    Last = lists:last(gb_trees:values(Best)),
    io:format(" ~w", [Last#point.value]),
    [{Size, Time}
    || #point{size = Size, time = Time} <- gb_trees:values(Best)].
