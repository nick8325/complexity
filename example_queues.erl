%% A queues example. Implements a double-ended queue.
%% Alternating removing from both ends causes bad time complexity.
-module(example_queues).
-compile(export_all).
-import(measure, [measure/6]).
-import(timing, [time1/1]).
-include_lib("eqc/include/eqc.hrl").

%% Argh! lists:reverse makes a constant number of reductions.
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
    [_X|Xs] = reverse(Ys),
    {Xs, []};
out1({[_X|Xs], Ys}) ->
    {Xs, Ys}.
out2({[], []}) ->
    error(empty_queue);
out2({Xs, []}) ->
    [_Y|Ys] = reverse(Xs),
    {[], Ys};
out2({Xs, [_Y|Ys]}) ->
    {Xs, Ys}.
empty({[], []}) ->
    true;
empty(_) ->
    false.
len({Xs, Ys}) ->
    length(Xs) + length(Ys).

initial() ->
    {[],[]}.
commands(Q, _) ->
    [cmd(in1, [a]), cmd(in2, [a])] ++
    [cmd(out1, []) || len(Q) > 0] ++
    [cmd(out2, []) || len(Q) > 0].
valid({call, _, X, _}, Q, _) when X == out1; X == out2 ->
    len(Q) > 0.
amortised() ->
    true.
cmd(X, Args) -> {call, ?MODULE, X, Args ++ [{var, value}]}.

measure_queue() ->
    commands:measure(5, 50, ?MODULE).
