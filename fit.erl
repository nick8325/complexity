%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/2]).

collate(Xs) -> collate(lists:sort(Xs), 1).
collate([], _) -> [];
collate([X], N) -> [{X, N}];
collate([X,X|Xs], N) -> collate([X|Xs], N+1);
collate([X,Y|Xs], N) -> [{X, N}|collate([Y|Xs], 1)].

graph(File, Points) ->
    file:write_file(File,
      [ io_lib:format("~p ~p ~p~n", [X, Y, 1]) || {X, Y} <- Points ]).

fit(Worst, Best) ->
    graph("worst", Worst),
    graph("best", Best),
    io:put_chars(os:cmd("./Fit && gnuplot -persist gnuplot")).
