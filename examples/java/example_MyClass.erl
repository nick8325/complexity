
-module(example_MyClass).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%% Convert command tuples to Java strings.
to_command({add, X}) ->
  io_lib:format("obj.add(~p);", [X]);
to_command({remove, X}) ->
  io_lib:format("obj.remove(~p);", [X]).

to_commands(Commands) ->
  ["{",
   "MyClass obj = new MyClass();",
   lists:map(fun to_command/1, Commands),
   "}"].


%% Evaluates commands in the model space.
eval_cmd_model({add, X}, Model) ->
  lists:usort([X | Model]);
eval_cmd_model({remove, X}, Model) ->
  Model -- [X].

eval_cmds_model([]) ->
  [];
eval_cmds_model([ Cmd | Cmds ]) ->
  eval_cmd_model(Cmd, eval_cmds_model(Cmds)). 


%% Converts the provided commands to Java and runs them.
eval_cmds(Cmds) ->
  RCmds = lists:reverse(Cmds),
  Commands = to_commands(RCmds),
  measure_java:run_java_commands(false, 1, 50, lists:flatten(Commands)).


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
measure_size(Cmds) ->
  length(Cmds).

measure() ->
  Family = #family{initial = [], grow = fun command_sequence/1},
  Axes = #axes{size = fun measure_size/1,
               time = fun eval_cmds/1,
               repeat = 2},
  {Time, _} = timer:tc(measure_java, measure_java, [1, 100, Family, Axes]),
  Time / 1000000.

