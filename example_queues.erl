%% A queues example. Implements a double-ended queue.
%% Alternating removing from both ends causes bad time complexity.
-module(example_queues).
-compile(export_all).
-include("measure.hrl").
-include_lib("eqc/include/eqc.hrl").

%% Generate and interpret "commands".
cmd(out, Q) -> element(2, queues:out(Q));
cmd(out_r, Q) -> element(2, queues:out_r(Q));
cmd({in, X}, Q) -> queues:in(X, Q);
cmd({in_r, X}, Q) -> queues:in_r(X, Q).

cmds(Cmds) -> cmds(Cmds, queues:new()).

cmds([], Q) -> Q;
cmds([Cmd|Cmds], Q) -> cmds(Cmds, cmd(Cmd, Q)).

valid_cmds(Cmds) -> valid_cmds(0, Cmds).

valid_cmds(_, []) -> true;
valid_cmds(0, [out|_]) -> false;
valid_cmds(0, [out_r|_]) -> false;
valid_cmds(N, [out|Cmds]) -> valid_cmds(N-1, Cmds);
valid_cmds(N, [out_r|Cmds]) -> valid_cmds(N-1, Cmds);
valid_cmds(N, [{in,_}|Cmds]) -> valid_cmds(N+1, Cmds);
valid_cmds(N, [{in_r,_}|Cmds]) -> valid_cmds(N+1, Cmds).

gen_cmds(Cmds) ->
    Candidates =
      example_sorting:insert_anywhere([out, out_r, {in,a}, {in_r,a}], Cmds),
    lists:filter(fun valid_cmds/1, Candidates).

measure_queues() ->
    measure(1, 50,
            #family{initial=[], grow=fun gen_cmds/1},
            #axes{size=fun length/1, time=time1(fun cmds/1)}).
