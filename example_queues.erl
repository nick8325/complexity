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

cmds(Cmds) ->
    cmds(Cmds, {[],[]}).
cmds([], Q) ->
    Q;
cmds([Cmd|Cmds], Q) ->
    cmds(Cmds, cmd(Cmd, Q)).

gen_cmds(Cmds) ->
    Q = cmds(Cmds),
    NewCmds =
      [ out1 || not empty(Q) ] ++
      [ out2 || not empty(Q) ] ++
        [ in1, in2 ],
    [ Cmds ++ [NewCmd] || NewCmd <- NewCmds ].

measure_queue() ->
    measure(20, 100,
            fun length/1,
            [],
            fun gen_cmds/1,
            fun cmds/1).
