
-module(java_test).


-include_lib("eqc/include/eqc.hrl").
-include("../measure.hrl").


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


%% Evaluates commands in the model space.
eval_cmd_model({add, X}, Model) ->
  lists:usort([X | Model]);
eval_cmd_model({remove, X}, Model) ->
  Model -- [X].

eval_cmds_model([]) ->
  [];
eval_cmds_model([ Cmd | Cmds ]) ->
  eval_cmd_model(Cmd, eval_cmds_model(Cmds)). 


%% Evaluates commands in the Java space.
eval_cmds(Cmds) ->
  RCmds = lists:reverse(Cmds),
  Commands = to_commands(RCmds),
  java:call_static(get_java_node(), 'Complexity', run, [true, 1, 50, lists:flatten(Commands)]).

to_command({add, X}) ->
  io_lib:format("obj.add(~p);", [X]);
to_command({remove, X}) ->
  io_lib:format("obj.remove(~p);", [X]).

to_commands(Commands) ->
  ["{",
   "MyClass obj = new MyClass();",
   lists:map(fun to_command/1, Commands),
   "}"].


%% Command sequence generators.
cmds(Model) ->
  [ {add, resize(100, int())} ] ++
  [ {remove, 0} ] ++
  [ {remove, elements(Model)} || length(Model) > 0 ].

command_sequence(Cmds) ->
  Model = eval_cmds_model(Cmds),
  ?LET(NewCmds, cmds(Model),
%      return([ [NewCmd | Cmds] || NewCmd <- NewCmds ])).
     return([ [NewCmd | Cmds] || NewCmd <- NewCmds ] ++
            [ Cmds ++ [NewCmd] || NewCmd <- NewCmds ] )).
%    return(example_sorting:insert_anywhere(NewCmds, Cmds))).


%% Measure functions.
measure_grow(Cmds) ->
  command_sequence(Cmds).

measure_size(Cmds) ->
  length(Cmds).
%  length(eval_cmds_model(Cmds)).

measure_time(Cmds) ->
  eval_cmds(Cmds).

measure_measure(Cmds) ->
  Model = eval_cmds_model(Cmds),
  length(Model).   


measure() ->
  start_java_node(),
  Family = #family{initial = [], grow = fun measure_grow/1},
  Axes = #axes{size = fun measure_size/1,
               time = fun measure_time/1,
%              measurements = [ fun measure_measure/1 ],
               repeat = 2},
  {Time, _} = timer:tc(measure, measure, [1, 100, Family, Axes]),
  Time / 1000000.


print_object(Object) ->
  io:format("===========================~n"),
  io:format(">>> Object: ~p~n", [Object]),
  Class = java:find_class(Object),
  io:format(">>> Class: ~p~n", [Class]),
  io:format("===========================~n").


test() ->
  try
    {ok, Node} = java:start_node([{java_verbose, "WARNING"}]),
    HashSet = java:new(Node, 'java.util.HashSet', []),
    print_object(HashSet),
    java:call(HashSet, add, [1]),
    java:call(HashSet, add, [2]),
    io:format(">>> Contains 1: ~p~n", [java:call(HashSet, contains, [1])]),
    io:format(">>> Contains 2: ~p~n", [java:call(HashSet, contains, [2])]),
    io:format(">>> Contains 3: ~p~n", [java:call(HashSet, contains, [3])]),
    Elements = java:call(HashSet, toArray, []),
    print_object(Elements),
    io:format(">>> ~p~n", [java:array_to_list(Elements)]),
    ok
  catch {java_exception, E} ->
    erlang:display(erlang:get_stacktrace()),
    java:print_stacktrace(E)
  end.

