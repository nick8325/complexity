%% Unbalanced binary search trees.
-module(example_trees).
-compile(export_all).
-import(measure, [measure/6]).
-import(timing, [time1/1]).
-include_lib("eqc/include/eqc.hrl").

member(X, {node, _, X, _}) ->
    true;
member(X, {node, L, Y, _})
  when X < Y ->
    member(X, L);
member(X, {node, _, _, R}) ->
    member(X, R);
member(_, _) ->
    false.

insert(X, T={node, _, X, _}) ->
    T;
insert(X, {node, L, Y, R})
  when X < Y ->
    {node, insert(X, L), Y, R};
insert(X, {node, L, Y, R}) ->
    {node, L, Y, insert(X, R)};
insert(X, nil) ->
    {node, nil, X, nil}.

insert_many([], T) ->
    T;
insert_many([X|Xs], T) ->
    insert_many(Xs, insert(X, T)).

tree_size(nil) ->
    0;
tree_size({node, L, _, R}) ->
    tree_size(L) + 1 + tree_size(R).

gen_tree({X, Xs, _T}) ->
  ?LET(Y, resize(100, int()),
  begin
      Cands = 
          [ {X, Ys} || Ys <- example_sorting:insert_anywhere(Y, Xs) ] ++
          [ {Y, Xs} ] ++
          [ {Y, Ys} || Ys <- example_sorting:insert_anywhere(X, Xs) ],
      return(
        [ {Z, Zs, insert_many(Zs, nil)} || {Z, Zs} <- Cands ])
  end).

measure_trees() ->
    measure(5, 50,
            fun({_, _, T}) -> tree_size(T) end,
            {0, [], nil},
            fun gen_tree/1,
            time1(fun({X, _Xs, T}) -> insert(X, T) end)).
