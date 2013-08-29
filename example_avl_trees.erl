%% AVL trees, but with normal BST deletion.
-module(example_avl_trees).
-compile(export_all).
-import(measure, [measure/6]).
-include_lib("eqc/include/eqc.hrl").

-record(node, {left, value, right}).

node(L={HL,_}, X, R={HR,_}) ->
  {1 + max(HL, HR),
   #node{left = L, value = X, right = R}}.

member(X, {_, #node{value = X}}) ->
    true;
member(X, {_, #node{left = L, value = Y}})
  when X < Y ->
    member(X, L);
member(X, {_, #node{right = R}}) ->
    member(X, R);
member(_, _) ->
    false.

insert(X, T={_,#node{value = X}}) ->
    T;
insert(X, {_,#node{left = L, value = Y, right = R}})
  when X < Y ->
    balance(node(insert(X, L), Y, R));
insert(X, {_,#node{left = L, value = Y, right = R}}) ->
    balance(node(L, Y, insert(X, R)));
insert(X, {_,nil}) ->
    node({0,nil}, X, {0,nil}).

delete(_, T={_,nil}) ->
  T;
delete(X, {_,#node{left = L, value = Y, right = R}})
  when X < Y ->
    node(delete(X, L), Y, R);
delete(X, {_,#node{left = L, value = Y, right = R}})
  when X > Y ->
    node(L, Y, delete(X, R));
delete(X, {_,#node{left = {_,nil}, value = X, right = R}}) ->
  R;
delete(X, {_,#node{left = L, value = X, right = R}}) ->
  Y = greatest(L),
  node(delete(Y, L), Y, R).

greatest({_,#node{value = X, right = {_,nil}}}) ->
  X;
greatest({_,#node{right = R}}) ->
  greatest(R).

balance(
  {_,#node{
     left = {_,#node{left = L = {HL,_}, value = X, right = M}},
     value = Y, right = R = {HR,_}}})
  when HL - HR == 1 ->
    node(L, X, node(M, Y, R));
balance(
  {_,#node{
     left = {_, #node{left = L, value = X,
                      right = {HM, #node{left = ML, value = Y, right = MR}}}},
     value = Z, right = R = {HR, _}}})
  when HM - HR == 1 ->
    node(node(L, X, ML), Y, node(MR, Z, R));
balance(
  {_, #node{
     left = L = {HL, _}, value = X,
     right = {_, #node{left = M, value = Y, right = R = {HR, _}}}}})
  when HR - HL == 1 ->
    node(node(L, X, M), Y, R);
balance(
  {_, #node{
     left = L = {HL, _}, value = X,
     right = {_, #node{left = {HM, #node{left = ML, value = Y, right = MR}},
                   value = Z, right = R}}}})
  when HM - HL == 1 ->
    node(node(L, X, ML), Y, node(MR, Z, R));
balance(T) ->
  T.

tree_size({_,nil}) ->
    0;
tree_size({_,{node, L, _, R}}) ->
    tree_size(L) + 1 + tree_size(R).

keys({_,nil}) ->
  [];
keys({_,{node, L, X, R}}) ->
  keys(L) ++ [X] ++ keys(R).

cmds(T) ->
  [{insert, resize(100, int())} || tree_size(T) =< 50 ] ++
  [{delete, elements(keys(T))} || tree_size(T) > 0].

eval_cmd({insert, X}, T) ->
  insert(X, T);
eval_cmd({delete, X}, T) ->
  delete(X, T).

eval_cmds([], T) ->
  T;
eval_cmds([X|Xs], T) ->
  eval_cmd(X, eval_cmds(Xs, T)).

construct([Cmd|Cmds]) ->
  {Cmd, Cmds, eval_cmds(Cmds, {0,nil})}.

gen_tree({Cmd, Cmds, T}) ->
  T1 = eval_cmd(Cmd, T),
  ?LET(Cmds1, cmds(T1),
  return(
  [ construct(Cmds2)
  || Cmd1 <- Cmds1,
     Cmds2 <- example_sorting:insert_anywhere(Cmd1, [Cmd|Cmds]) ])).

measure_avl_trees() ->
    measure(20, 500,
            fun({_, _, T}) -> tree_size(T) end,
            construct([{insert,0}]),
            fun gen_tree/1,
            fun({X, _Xs, T}) -> eval_cmd(X, T) end).
