%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/1]).

collate(Xs) -> collate(lists:sort(Xs), 1).
collate([], _) -> [];
collate([X], N) -> [{X, N}];
collate([X,X|Xs], N) -> collate([X|Xs], N+1);
collate([X,Y|Xs], N) -> [{X, N}|collate([Y|Xs], 1)].

graph(Points) ->
    file:write_file("data",
      [ io_lib:format("~p ~p ~p~n", [X, Y, K]) || {{X, Y}, K} <- Points ]).

fit(Points) ->
    graph(collate(Points)),
    io:put_chars(os:cmd("./Fit && gnuplot -persist gnuplot")).
