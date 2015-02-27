
-module(example_MyClass).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%%
%% This module shows how to test the complexity of the Java class MyClass that
%% is a simple implementation of a set that allows for duplicated elements.
%%
%% The Java class provides two methods:
%%   void add(int x) - Adds the provided integer to the set.
%%   boolean remove(int x) - Removes the first occurrence of the provided integer
%%                           from the set.
%%


%%
%% To test the complexity we generate sequences of abstract add and remove commands
%% and do so by implementing two command generators:
%%   cmds - Generates a new command given the list of elements in the set.
%%   command_sequence - Takes a sequence of commands and returns a list of
%%                      sequences where a new command has been inserted in
%%                      the sequence.
%%

%% Returns a new command given a model (i.e., the list of current elements in the set).
%%
%% The new command either adds a random element, removes the element 0 (even if it
%% doesn't occur in the set) or removes one of the elements in the set.
cmds(Model) ->
  [ {add, resize(100, int())} ] ++
  [ {remove, 0} ] ++
  [ {remove, elements(Model)} || length(Model) > 0 ].

%% Returns a list of command sequences that have been extended with a new command
%% at the beginning or the end.
command_sequence(Cmds) ->
  Model = eval_cmds_model(Cmds),
  ?LET(NewCmds, cmds(Model),
    return([ [NewCmd | Cmds]  || NewCmd <- NewCmds ] ++
           [ Cmds ++ [NewCmd] || NewCmd <- NewCmds ] )).

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
  io_lib:format("obj.add(Integer.valueOf(~p));", [X]);
to_command({remove, X}) ->
  io_lib:format("obj.remove(Integer.valueOf(~p));", [X]).

%% Convert the commands to a list of Java code strings.
to_commands(Commands) ->
  ["{",
     "MyClass obj = new MyClass();",
     lists:map(fun to_command/1, Commands),
   "}"].

%% Evaluate the Java commands.
%%
%% The arguments to run_java_commands are:
%%  - A boolean telling if the Java runtime should be garbage collected
%%    before running the tests.
%%  - The number of times to run the commands.
%%  - The Java code string to run.
eval_cmds(Cmds) ->
  RCmds = lists:reverse(Cmds),
  Commands = to_commands(RCmds),
  measure_java:run_java_commands(false, 50, null, lists:flatten(Commands)).


%%
%% To infer the complexity we call measure_java:measure_java and pass the
%% functions for generating command sequences, measuring the input size and
%% evaluating the commands.
%%

%% We let the input size be the number of commands in the command sequence.
measure_size(Cmds) ->
  length(Cmds).

measure() ->
  Family = #family{initial = [], grow = fun command_sequence/1},
  Axes = #axes{size = fun measure_size/1,
               time = fun eval_cmds/1,
               repeat = 5},
  {Time, _} = timer:tc(measure_java, measure_java, [1,  100, Family, Axes]),
  Time / 1000000.

