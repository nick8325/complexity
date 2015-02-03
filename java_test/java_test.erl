
-module(java_test).


-include_lib("eqc/include/eqc.hrl").
-include("../measure.hrl").


-compile(export_all).


%% Utility functions.
get_java_node() ->
  get(java_node).

set_java_node(Node) ->
  put(java_node, Node).

get_test_obj() ->
  get(test_obj).

set_test_obj(Obj) ->
  put(test_obj, Obj).


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

%eval_cmds([]) ->
%  java:new(get_java_node(), 'MyClass', []);
%eval_cmds([ Cmd | Cmds ]) ->
%  eval_cmd(Cmd, eval_cmds(Cmds)). 
eval_cmds(Cmds) ->
%  {ok, Node} = java:start_node([{java_verbose, "WARNING"},
%                                {add_to_java_classpath,["."]}]),
%  TestObj = java:new(Node, 'MyTest', []),
  TestObj = java:new(get_java_node(), 'MyTest', []),
  set_test_obj(TestObj),
  RCmds = lists:reverse(Cmds),

  Commands = to_commands(RCmds),
  Result = java:call(get_test_obj(), run, [1, 50, lists:flatten(Commands)]),
  %java:terminate(Node),
  Result.

to_commands(Commands) ->
  ["{",
   "MyClass obj = new MyClass();",
   lists:map(fun to_command/1, Commands),
   "}"].

to_command({add, X}) ->
  io_lib:format("obj.add(~p);", [X]);
to_command({remove, X}) ->
  io_lib:format("obj.remove(~p);", [X]).


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
%%  apply(time1(fun eval_cmds/1), [Cmds]).
%  Times = [ begin {Time, _} = timer:tc(?MODULE, eval_cmds, [Cmds]), Time end || _ <- lists:seq(1, 5) ], 
%  lists:min(Times).
  eval_cmds(Cmds).

measure_measure(Cmds) ->
  Model = eval_cmds_model(Cmds),
  length(Model).   

start_java_node() ->
  {ok, Node} = java:start_node([{java_verbose, "WARNING"},
                                {add_to_java_classpath,["."]}]),
  set_java_node(Node).

warm_up_java() ->
  eval_cmds([{add, 0}]),
  ok.  

measure() ->
  start_java_node(),
  warm_up_java(),
  measure(1, 100,
          #family{initial = [], grow = fun measure_grow/1},
          #axes{size = fun measure_size/1,
                time = fun measure_time/1,
%                measurements = [ fun measure_measure/1 ],
                repeat = 2}).


print_object(Object) ->
  io:format("===========================~n"),
  io:format(">>> Object: ~p~n", [Object]),
  Class = java:find_class(Object),
  io:format(">>> Class: ~p~n", [Class]),
  io:format("===========================~n").


test_MyClass() ->
  {ok, Node} = java:start_node([{java_verbose, "WARNING"},
                                {add_to_java_classpath,["."]}]),
  MyClass = java:new(Node, 'MyClass', []),
  print_object(MyClass).


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

test2() ->
  start_java_node(),
  Test = java:new(get_java_node(), 'MyTest', []),
  java:call(Test, run, [100, ["add", "remove"], [0, 1]]).
%  java:call(Test, run, [[{"a", "a_arg"}, {"b", "b_arg"}, {"c", "c_arg"}]]).

