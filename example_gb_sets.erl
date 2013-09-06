%% A gb_sets example.
-module(example_gb_sets).
-compile(export_all).
-import(measure, [measure/6]).
-import(timing, [time1/1]).
-include_lib("eqc/include/eqc.hrl").

measure_lookup_gbsets() ->
    measure(20, 500,
            fun({_, T}) -> gb_sets:size(T) end,
            {0, gb_sets:new()},
            fun gb_sets_gen/1,
            time1(fun({X, T}) -> gb_sets:is_element(X, T) end)).

gb_sets_gen({X, T}) ->
  ?LET(Y, resize(1000, int()),
       return(
       [ {X, gb_sets:add(Y, T)},
         {Y, T},
         {Y, gb_sets:add(Y, T)} ])).

cmds(T) ->
  [{insert, resize(100, int())} ] ++
  [{delete, elements(gb_sets:to_list(T))} || gb_sets:size(T) > 0].

eval_cmd({insert, X}, T) ->
  gb_sets:add(X, T);
eval_cmd({delete, X}, T) ->
  gb_sets:delete_any(X, T).

eval_cmds([], T) ->
  T;
eval_cmds([X|Xs], T) ->
  eval_cmd(X, eval_cmds(Xs, T)).

construct([Cmd|Cmds]) ->
  {Cmd, Cmds, eval_cmds(Cmds, gb_sets:empty())}.

gen_tree({Cmd, Cmds, T}) ->
  T1 = eval_cmd(Cmd, T),
  ?LET(Cmds1, cmds(T1),
  return(
  [ construct(Cmds2)
  || Cmd1 <- Cmds1,
     Cmds2 <- example_sorting:insert_anywhere(Cmd1, [Cmd|Cmds]) ])).

is_insert({insert, _}) ->
  true;
is_insert(_) ->
  false.

measure_gb_sets() ->
    measure(5, 100,
            fun({_, Xs, _}) -> length(Xs) end,
            construct([{insert,0}]),
            fun gen_tree/1,
            time1(fun({X, Xs, _T}) -> construct([X|Xs]) end)).
