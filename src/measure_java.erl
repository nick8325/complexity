
-module(measure_java).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%% Utility functions.
get_java_node() ->
  get(java_node).

set_java_node(Node) ->
  put(java_node, Node).

start_java_node(ClassPaths) ->
  {ok, Node} = java:start_node([{java_verbose, "WARNING"},
                                {add_to_java_classpath,[ "." | ClassPaths ]}]),
  set_java_node(Node).

stop_java_node() ->
  java:terminate(get_java_node()).


%% Runs the provided java command strings and returns the runtime in ms.
run_java_commands(GC, Iterations, SetupCmdsString, CmdsString, TeardownCmdsString) ->
  java:call_static(get_java_node(), 'Complexity', measure, [GC, Iterations, SetupCmdsString, CmdsString, TeardownCmdsString]) / 1000000.


%% Runs the provided function in a Java environment.
%% I.e., it will make sure that a Java node is started
%% before running the function.
with_java(ClassPaths, RunFun) ->
  start_java_node(ClassPaths),
  Result = RunFun(),
  stop_java_node(),
  Result.


%% Measure functions.
measure_java(Rounds, MaxSize, Family, Axes, ClassPaths) ->
  measure_java(Rounds, MaxSize, Family, Axes, ClassPaths, fun() -> ok end, fun() -> ok end).

measure_java(Rounds, MaxSize, Family, Axes, ClassPaths, SetupFun, TeardownFun) ->
  RunFun =
    fun() ->
      SetupFun(),
      WarmUpGrow = fun(X) -> ?LET(Lst, (Family#family.grow)(X), [eqc_gen:oneof(Lst)]) end,
      io:format("Warming up the Java VM...~n"),
      measure(1, MaxSize, Family#family{ grow = WarmUpGrow, warmup = true }, Axes),
      io:format("~nRunning the complexity benchmarks...~n"),
      Result = measure(Rounds, MaxSize, Family, Axes),
      TeardownFun(),
      Result
    end,
  eqc:start(),
  with_java(ClassPaths, RunFun).

