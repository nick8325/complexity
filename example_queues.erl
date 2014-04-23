%% A queues example. Implements a double-ended queue.
%% Alternating removing from both ends causes bad time complexity.
-module(example_queues).
-compile(export_all).
-import(measure, [measure/4]).
-import(timing, [time1/1]).
-include("measure.hrl").
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
cmd({out,1}, Q) ->
    element(2, out1(Q));
cmd({out,2}, Q) ->
    element(2, out2(Q));
cmd({in,1}, Q) ->
    in1(a, Q);
cmd({in,2}, Q) ->
    in2(a, Q).

cmds(Cmds) ->
    cmds(Cmds, {[],[]}).
cmds([], Q) ->
    Q;
cmds([Cmd|Cmds], Q) ->
    cmds(Cmds, cmd(Cmd, Q)).

valid_cmds(Cmds) ->
    valid_cmds(0, Cmds).
valid_cmds(_, []) ->
    true;
valid_cmds(0, [{out,_}|_]) ->
    false;
valid_cmds(N, [{out,_}|Cmds]) ->
    valid_cmds(N-1, Cmds);
valid_cmds(N, [{in,_}|Cmds]) ->
    valid_cmds(N+1, Cmds).

gen_cmds(Cmds) ->
    Candidates =
      example_sorting:insert_anywhere({out,1}, Cmds) ++
      example_sorting:insert_anywhere({out,2}, Cmds) ++
      example_sorting:insert_anywhere({in,1}, Cmds) ++
      example_sorting:insert_anywhere({in,2}, Cmds),
    lists:filter(fun valid_cmds/1, Candidates).

measure_queue() ->
    measure(1, 50,
            #family{initial=[], grow=fun gen_cmds/1},
            #axes{size=fun length/1, time=time1(fun cmds/1)}).
