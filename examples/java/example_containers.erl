
-module(example_containers).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%%
%% This module is based on example_MyClass.erl and shows how to test the
%% complexity of various Java container classes.
%%


cmds(Model) ->
  [ {add, resize(100, int())} ] ++
  [ {remove, 0} ] ++
  [ {remove, elements(Model)} || length(Model) > 0 ].

normalise(Cmds) ->
  normalise(Cmds, dict:new()).

normalise([], _) -> [];
normalise([ {Cmd, N} | Cmds ], Dict) ->
  {N2, Dict2} =
    case dict:find(N, Dict) of
      {ok, [X]} -> {X, Dict};
      error ->
        X = dict:size(Dict),
        {X, dict:append(N, X, Dict)}
    end,
  [ {Cmd, N2} | normalise(Cmds, Dict2) ].

%% Returns a list of command sequences that have been extended with a new command
%% at the beginning or the end.
command_sequence(Cmds) ->
  Model = eval_cmds_model(Cmds),
  ?LET(NewCmds, cmds(Model),
    return([ normalise([NewCmd | Cmds])  || NewCmd <- NewCmds ] ++
           [ normalise(Cmds ++ [NewCmd]) || NewCmd <- NewCmds ] )).

%% Evaluates the commands and returns a list of the current elements.
eval_cmds_model([]) ->
  [];
eval_cmds_model([ Cmd | Cmds ]) ->
  eval_cmd_model(Cmd, eval_cmds_model(Cmds)).

eval_cmd_model({add, X}, Model) ->
  [X | Model];
eval_cmd_model({remove, X}, Model) ->
  Model -- [X].


%%
%% To run the abstract commands as Java commands we convert them to string
%% representations of the Java code and call measure_java:run_java_commands.
%%

%% Convert command tuples to Java strings.
to_command({add, X}) ->
  io_lib:format("container.add(Integer.valueOf(~p));", [X]);
to_command({remove, X}) ->
  io_lib:format("container.remove(Integer.valueOf(~p));", [X]).

%% Convert the commands to a list of Java code strings.
to_commands(ContainerType, Commands) ->
  ["{",
     ContainerType ++ " container = new " ++ ContainerType ++ "();",
     lists:map(fun to_command/1, Commands),
   "}"].

%% Evaluate the Java commands.
%%
%% The arguments to run_java_commands are:
%%  - A boolean telling if the Java runtime should be garbage collected
%%    before running the tests.
%%  - The number of times to run the commands.
%%  - The Java code string to run.
eval_cmds(ContainerType, Cmds) ->
  RCmds = lists:reverse(Cmds),
  Commands = to_commands(ContainerType, RCmds),
  measure_java:run_java_commands(false, 50, lists:flatten(Commands)).


%%
%% To infer the complexity we call measure_java:measure_java and pass the
%% functions for generating command sequences, measuring the input size and
%% evaluating the commands.
%%

%% We let the input size be the number of commands in the command sequence.
measure_size(Cmds) ->
  length(Cmds).

measure(ContainerType) ->
  Family = #family{initial = [], grow = fun command_sequence/1},
  Axes = #axes{size = fun measure_size/1,
               time = fun(Cmds) -> eval_cmds(ContainerType, Cmds) end,
               repeat = 5},
  {Time, _} = timer:tc(measure_java, measure_java, [1, 100, Family, Axes]),
  Time / 1000000.

measure_ArrayList() -> measure("java.util.ArrayList").
measure_ArrayListWrapper() -> measure("ArrayListWrapper").
measure_HashSet() -> measure("java.util.HashSet").
measure_MyClass() -> measure("MyClass").
measure_TreeSet() -> measure("java.util.TreeSet").

