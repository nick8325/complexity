-module(example_noise).

-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").

-compile(export_all).



measure_grow(N) ->
  [N + 1].

measure_size(N) ->
  N.

measure_time(N) ->
  N + random:uniform(100).

measure() ->
  Family = #family{initial = 1, grow = fun measure_grow/1},
  Axes = #axes{size = fun measure_size/1,
               time = fun measure_time/1,
               repeat = 100},
  {Time, _} = timer:tc(measure, measure, [1, 100, Family, Axes]),
  Time.
 
