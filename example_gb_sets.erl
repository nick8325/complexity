%% A gb_sets example.
-module(example_gb_sets).
-compile(export_all).
-import(measure, [measure/6]).
-include_lib("eqc/include/eqc.hrl").

measure_lookup_gbsets() ->
    measure(20, 500,
            fun({_, T}) -> gb_sets:size(T) end,
            {0, gb_sets:new()},
            fun gb_sets_gen/1,
            fun({X, T}) -> gb_sets:is_element(X, T) end).

gb_sets_gen({X, T}) ->
  ?LET(Y, resize(1000, int()),
       return(
       [ {X, gb_sets:add(Y, T)},
         {Y, T},
         {Y, gb_sets:add(Y, T)} ])).
