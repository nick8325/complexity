-module(example_jsx_gedell).

-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").

-compile(export_all).



%% Measures the size of the structure.
measure_size(Lst) ->
%  length(json_gen:paths(Lst)).
  length(binary:bin_to_list(jsx:encode(Lst))).

time_encode(Lst) ->
  timing:time(fun jsx:encode/1, [Lst]).

time_decode(Lst) ->
  Json = jsx:encode(Lst),
  timing:time(fun jsx:decode/1, [Json]).

time_prettify(Lst) ->
  Json = jsx:encode(Lst),
  timing:time(fun jsx:prettify/1, [Json]).

measure(TimeFun) ->
  Family = #family{initial = json_gen:empty(), grow = fun json_gen:grow_random/1},
  Axes = #axes{size = fun measure_size/1,
               time = TimeFun,
               measurements = [ fun json_gen:max_depth/1 ],
               repeat = 2},
  {Time, _} = timer:tc(measure, measure, [1, 800, Family, Axes]),
  Time / 1000000.
 
measure_encode() ->
  measure(fun time_encode/1).

measure_decode() ->
  measure(fun time_decode/1).

measure_prettify() ->
  measure(fun time_prettify/1).

