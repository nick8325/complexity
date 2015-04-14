%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/4]).
-include("measure.hrl").

fit(Name, Outliers, WorstPoints, BestPoints) ->
  file:write_file("data_worst_" ++ Name, [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- WorstPoints ]),
  file:write_file("data_best_" ++ Name, [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- BestPoints ]),
  io:put_chars(os:cmd(io_lib:format("./Fit ~p ~p && GNUTERM=pdfcairo gnuplot gnuplot_~p > plot_~p.pdf", [Name, Outliers, Name, Name]))),
  io:put_chars(os:cmd(io_lib:format("GNUTERM=pdfcairo gnuplot gnuplot_raw_~p > plot_raw_~p.pdf", [Name, Name]))).
