
-module(example_json_simple).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


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
  measure_java:run_java_commands(false, 50, lists:flatten(Commands)).


%%
%% To infer the complexity we call measure_java:measure_java and pass the
%% functions for generating command sequences, measuring the input size and
%% evaluating the commands.
%%

%% Measures the size of the structure.
measure_size(Lst) ->
  length(json_gen:paths(Lst)).

escape_string(Str) ->
  io_lib:format("~p", [Str]).


time_encode(Lst) ->
  Json = binary:bin_to_list(jsx:encode(Lst)),
  SetupCommands =
    [
      "{",
        "org.json.simple.parser.JSONParser parser = new org.json.simple.parser.JSONParser();",
        "return parser.parse(" ++ escape_string(Json) ++ ");",
      "}"
    ],
  RunCommands = 
    [
      "{",
        "$1.toString();",
      "}"
    ],
  measure_java:run_java_commands(false, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


time_decode(Lst) ->
  Json = binary:bin_to_list(jsx:encode(Lst)),
  SetupCommands =
    [
      "{",
        "return new Object[]{new org.json.simple.parser.JSONParser(), " ++ escape_string(Json) ++ "};",
      "}"
    ],
  RunCommands = 
    [
      "{",
        "Object[] args = (Object[])$1;",
        "org.json.simple.parser.JSONParser parser = (org.json.simple.parser.JSONParser)args[0];",
        "Object parsed = parser.parse((String)args[1]);",
      "}"
    ],
  measure_java:run_java_commands(false, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


measure(TimeFun) ->
  Family = #family{initial = json_gen:empty(), grow = fun json_gen:grow_all/1},
  Axes = #axes{size = fun measure_size/1,
               time = TimeFun,
               repeat = 2},
  {Time, _} = timer:tc(measure_java, measure_java, [1,  50, Family, Axes]),
  Time / 1000000.


measure_encode() ->
  measure(fun time_encode/1).

measure_decode() ->
  measure(fun time_decode/1).

