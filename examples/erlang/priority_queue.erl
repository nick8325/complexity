%% A priority queue, used in measure.erl.
%% Implemented using a leftist heap.
-module(priority_queue).
-export([new/0, find_min/1, remove_min/1, insert/2]).

-record(node, {npl, value, left, right}).

new() ->
    nil.

npl(nil) ->
    0;
npl(#node{npl = N}) ->
    N.

node(Value, Left, Right) ->
    #node{npl = 1 + min(npl(Left), npl(Right)),
          value = Value,
          left = Left,
          right = Right}.

find_min(nil) ->
    empty;
find_min(#node{value = Value}) ->
    Value.

remove_min(nil) ->
    empty;
remove_min(#node{value = Value, left = Left, right = Right}) ->
    {found, Value, merge(Left, Right)}.

insert(X, H) ->
    merge(node(X, nil, nil), H).

merge(H, nil) ->
    H;
merge(nil, H) ->
    H;
merge(H1 = #node{value = X},
      H2 = #node{value = Y}) when X > Y ->
    merge(H2, H1);
merge(#node{value = X, left = L, right = R}, H) ->
    case npl(L) =< npl(R) of
        true ->
            node(X, merge(L, H), R);
        false ->
            node(X, L, merge(R, H))
    end.


