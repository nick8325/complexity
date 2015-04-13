%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/3]).
-include("measure.hrl").

fit(Outliers, WorstPoints, BestPoints) ->
  file:write_file("data_worst", [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- WorstPoints ]),
  file:write_file("data_best", [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- BestPoints ]),
  io:put_chars(os:cmd(io_lib:format("./Fit ~p && GNUTERM=pdfcairo gnuplot gnuplot > plot.pdf", [Outliers]))),
  io:put_chars(os:cmd("GNUTERM=pdfcairo gnuplot gnuplot_raw > plot_raw.pdf")).
