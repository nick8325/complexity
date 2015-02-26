-module(queues).
-include("queues.hrl").
-compile(export_all).


new() ->
  #queue{front = [], back = []}.


size(Q) ->
  length(Q#queue.front) + length(Q#queue.back).


in(Y, #queue{front = Xs, back = Ys}) ->
  #queue{front = Xs, back = [Y|Ys]}.


in_r(X, #queue{front = Xs, back = Ys}) ->
  #queue{front = [X|Xs], back = Ys}.


out(#queue{front = [], back = []}) ->
  error(empty_queue);
out(#queue{front = [], back = Ys}) ->
  out(#queue{front = reverse(Ys), back = []});
out(#queue{front = [X|Xs], back = Ys}) ->
  {X, #queue{front = Xs, back = Ys}}.


out_r(#queue{front = [], back = []}) ->
  error(empty_queue);
out_r(#queue{front = Xs, back = []}) ->
  out_r(#queue{front = [], back = reverse(Xs)});
out_r(#queue{front = Xs, back = [Y|Ys]}) ->
  {Y, #queue{front = Xs, back = Ys}}.


%% Argh! lists:reverse makes a constant number of reductions.
reverse(Xs) ->
    reverse(Xs, []).
reverse([], Ys) ->
    Ys;
reverse([X|Xs], Ys) ->
    reverse(Xs, [X|Ys]).

