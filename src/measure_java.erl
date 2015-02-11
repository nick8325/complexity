
-module(measure_java).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%% Utility functions.
get_java_node() ->
  get(java_node).

set_java_node(Node) ->
  put(java_node, Node).

start_java_node() ->
  {ok, Node} = java:start_node([{java_verbose, "WARNING"},
                                {add_to_java_classpath,["."]}]),
  set_java_node(Node).

stop_java_node() ->
  java:terminate(get_java_node()).


%% Runs the provided java command string.
run_java_commands(GC, Iterations, CmdsString) ->
  java:call_static(get_java_node(), 'Complexity', run, [GC, Iterations, CmdsString]).


%% Measure functions.
measure_java(Rounds, MaxSize, Family, Axes) ->
  start_java_node(),
  Result = measure(Rounds, MaxSize, Family, Axes),
  stop_java_node(),
  Result.

