%% A queues example.
-module(example_queues).
-compile(export_all).
-import(measure, [measure/6]).
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

%% Generate and interpret "commands".
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
    measure(20, 100,
            fun({_Cmd, Q}) -> len(Q) end,
            {in1, {[],[]}},
            fun queue_gen/1,
            fun({Cmd, Q}) -> cmd(Cmd, Q) end).
