-module(test).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

time(F) ->
    {reductions, A} = process_info(self(), reductions),
    F(),
    {reductions, B} = process_info(self(), reductions),
    B - A.

time_sort() ->
    Self = self(),
    Print = fun(X) -> Self ! {print, X} end,
    eqc:quickcheck(eqc:numtests(10000, eqc_gen:resize(1000,
    ?FORALL(Xs, list(int()),
    begin
      erlang:garbage_collect(),
      collect(Print, {length(Xs), time(fun() -> lists:sort(Xs) end)}, true)
    end)))),
    receive {print, X} -> X end.

graph(Points) ->
    %% XVals = [ X || {{X, _}, _} <- Points ],
    %% YVals = [ Y || {{_, Y}, _} <- Points ],
    %% XMin = lists:min(XVals),
    %% XMax = lists:max(XVals),
    %% YMin = lists:min(YVals),
    %% YMax = lists:max(YVals),
    %% file:write_file("gnuplot",
    %%   ["set output \"graph.png\"\n",
    %%    io_lib:format("plot 'data' with points lt 1 pt 6 ps variable~n", [XMin, XMax, YMin, YMax])]),
    file:write_file("data",
      [ io_lib:format("~p ~p ~p~n", [X, Y, K]) || {{X, Y}, K} <- Points ]).

    
