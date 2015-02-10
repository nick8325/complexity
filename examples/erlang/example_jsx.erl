-module(example_jsx).
-compile(export_all).
-compile({no_auto_import, [size/1]}).
-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").

%% An incremental list generator.
list_gen(Xs) ->
    ?LET(Ys, vector(5, resize(100, int())),
    ?LET(Zs, vector(5, elements(non_empty(0, Xs))),
      return(insert_anywhere(Ys ++ Zs, Xs)))).

non_empty(X, []) ->
    [X];
non_empty(_X, Xs) ->
    Xs.

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

%splits(Xs) ->
%    [{[], Xs}, {Xs, []}].

insert_anywhere(Xs, Ys) ->
    [ As ++ [X] ++ Bs || X <- Xs, {As, Bs} <- splits(Ys) ].

to_erlang({object, []}) ->
    [{}];
to_erlang({object, Xs}) ->
    [{Key, to_erlang(Value)} || {Key, Value} <- Xs];
to_erlang(Xs) when is_list(Xs) ->
    lists:map(fun to_erlang/1, Xs);
to_erlang(X) when not is_tuple(X) ->
    X.

object() ->
    {object, []}.

key() ->
    ?LET(X, list(elements(lists:seq($a,$z))), list_to_atom(X)).

measure_jsx(F, G) ->
    measure(1, 100,
            #family{initial = object(), grow = fun term_gen/1},
            #axes{size = fun size/1,
                  measurements = [],%fun depth/1],
                  time = fun(X) -> Y = F(to_erlang(X)), timing:time(G, [Y]) end}).

measure_encode() ->
    measure_jsx(fun(X) -> X end, fun jsx:encode/1).
measure_decode() ->
    measure_jsx(fun jsx:encode/1, fun jsx:decode/1).

size({object, Xs}) ->
    1 + lists:sum([ size(Value) || {_Key, Value} <- Xs]);
size(Xs) when is_list(Xs) ->
    1 + lists:sum(lists:map(fun size/1, Xs));
size(_) ->
    1.

depth({object, Xs}) ->
    1 + lists:max([0|[ depth(Value) || {_Key, Value} <- Xs]]);
depth(Xs) when is_list(Xs) ->
    1 + lists:max(lists:map(fun depth/1, Xs));
depth(_) ->
    1.

branchiness(X) ->
    branchiness(1, X).
branchiness(N, {object, Xs=[_|_]}) ->
    lists:sum([branchiness(N+1, X) || {_, X} <- Xs ]);
branchiness(N, Xs) when is_list(Xs) ->
    lists:sum([branchiness(N+1, X) || X <- Xs ]);
branchiness(N, _) ->
    N.

update({here, X}, _Y) ->
    X;
update({tuple, N, X}, Y) ->
    list_to_tuple(update({list, N-1, X}, tuple_to_list(Y)));
update({prepend, X}, Xs) ->
    X++Xs;
update({list, N, X}, Xs) ->
    {Ys, [Z|Zs]} = lists:split(N, Xs),
    Ys ++ [update(X, Z)] ++ Zs.

flatten(G={eqc_gen, _}) ->
    ?LET(X, G, flatten(X));
flatten({here, G}) ->
    ?LET(Xs, G, return([ {here, X} || X <- Xs ]));
flatten({tuple, N, G}) ->
    ?LET(Xs, flatten(G), return([ {tuple, N, X} || X <- Xs ]));
flatten({list, N, G}) ->
    ?LET(Xs, flatten(G), return([ {list, N, X} || X <- Xs ]));
flatten({prepend, G}) ->
    ?LET(X, G, return([{prepend, X}]));
flatten(Gs) when is_list(Gs) ->
    ?LET(Xss, lists:map(fun flatten/1, Gs), return(lists:concat(Xss))).

term_gen(X) ->
    ?LET(Us, flatten(updates(X)), [ update(U, X) || U <- Us ]).

remove_dups([]) ->
    [];
remove_dups([{K,V}|Xs]) ->
    [{K,V}|remove_dups([ {K1, V1} || {K1, V1} <- Xs, K /= K1 ])].

updates(X) ->
    [here(X), there(X)].
here({object, Xs}) ->
    Keys = [Key || {Key, _Value} <- Xs],
    {tuple, 2, {prepend, ?LET(Ys, non_empty(list({?SUCHTHAT(K, key(), not lists:member(K, Keys)), object()})), remove_dups(Ys))}};
here(Xs) when is_list(Xs) ->
    {prepend, list(object())};
here(_) ->
    [].
there({object, Xs}) ->
    {tuple, 2, [{list, I, {tuple, 2, updates(X)}} || {I, {_, X}} <- label(Xs)]};
there(Xs) when is_list(Xs) ->
    [ {list, I, updates(X)} || {I, X} <- label(Xs) ];
there(_) -> [].

label(Xs) ->
    lists:zip(lists:seq(0, length(Xs)-1), Xs).

funny(0) ->
    [];
funny(N) ->
    [funny(N-1)].
