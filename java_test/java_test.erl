
-module(java_test).


-include_lib("eqc/include/eqc.hrl").
-include("../measure.hrl").


-compile(export_all).


%% Utility functions.
get_java_node() ->
  get(java_node).

set_java_node(Node) ->
  put(java_node, Node).



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
eval_cmd({add, X}, Set) ->
  java:call(Set, add, [X]),
  Set;
eval_cmd({remove, X}, Set) ->
  java:call(Set, remove, [X]),
  Set.

eval_cmds([]) ->
  java:new(get_java_node(), 'java.util.HashSet', []);
eval_cmds([ Cmd | Cmds ]) ->
  eval_cmd(Cmd, eval_cmds(Cmds)). 


%% Command sequence generators.
cmds(Model) ->
  [ {add, resize(100, int())} || length(Model) < 5 ] ++
  [ {remove, elements(Model)} || length(Model) > 0 ].

command_sequence(Cmds) ->
  Model = eval_cmds_model(Cmds),
  ?LET(NewCmds, cmds(Model),
    return(example_sorting:insert_anywhere(NewCmds, Cmds))).


%% Measure functions.
measure_grow(Cmds) ->
  command_sequence(Cmds).

measure_size(Cmds) ->
  length(Cmds).
%  length(eval_cmds_model(Cmds)).

measure_time(Cmds) ->
  apply(time1(fun eval_cmds/1), [Cmds]).

start_java_node() ->
  {ok, Node} = java:start_node([{java_verbose, "WARNING"}]),
  set_java_node(Node).

warm_up_java() ->
  eval_cmds([{add, 0}]),
  ok.  

measure() ->
  start_java_node(),
  warm_up_java(),
  measure(1, 30,
          #family{initial = [], grow = fun measure_grow/1},
          #axes{size = fun measure_size/1, time = fun measure_time/1}).  


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


