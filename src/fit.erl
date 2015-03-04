%% Fit curves to a set of timing data.
%% The data should be in the form [{InputSize, Time}].
-module(fit).
-export([fit/2]).
-include("measure.hrl").

fit(Outliers, Points) ->
  file:write_file("data",
    [ io_lib:format("~p ~p~n", [X, Y]) || #point{coords=[X,Y|_]} <- Points ]),
  io:put_chars(os:cmd(io_lib:format("./Fit ~p && GNUTERM=pdfcairo gnuplot gnuplot > plot.pdf", [Outliers]))),
  io:put_chars(os:cmd("GNUTERM=pdfcairo gnuplot gnuplot_raw > plot_raw.pdf")).
