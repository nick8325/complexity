%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/1]).
-include("measure.hrl").

fit(Points) ->
  file:write_file("data",
    [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- Points ]),
  io:put_chars(os:cmd("./Fit && GNUTERM=pdfcairo gnuplot gnuplot > plot.pdf")).
