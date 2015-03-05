
-module(example_gson).


-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").


-compile(export_all).


%%
%% To infer the complexity we call measure_java:measure_java and pass the
%% functions for generating command sequences, measuring the input size and
%% evaluating the commands.
%%

%% Measures the size of the structure.
measure_size(Lst) ->
  length(json_gen:paths(Lst)).
%  length(binary:bin_to_list(jsx:encode(Lst))).

escape_string(Str) ->
  io_lib:format("~p", [Str]).


time_encode(Lst) ->
  Json = binary:bin_to_list(jsx:encode(Lst)),
  SetupCommands =
    [
      "{",
        "com.google.gson.Gson parser = new com.google.gson.Gson();",
        "return new Object[]{new com.google.gson.Gson(), parser.fromJson(" ++ escape_string(Json) ++ ", Object.class)};",
      "}"
    ],
  RunCommands = 
    [
      "{",
        "Object[] args = (Object[])$1;",
        "com.google.gson.Gson parser = (com.google.gson.Gson)args[0];",
        "parser.toJson(args[1]);",
      "}"
    ],
  measure_java:run_java_commands(true, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


time_decode(Lst) ->
  Json = binary:bin_to_list(jsx:encode(Lst)),
  SetupCommands =
    [
      "{",
        "return new Object[]{new com.google.gson.Gson(), " ++ escape_string(Json) ++ "};",
      "}"
    ],
  RunCommands = 
    [
      "{",
        "Object[] args = (Object[])$1;",
        "com.google.gson.Gson parser = (com.google.gson.Gson)args[0];",
        "Object parsed = parser.fromJson((String)args[1], Object.class);",
      "}"
    ],
  measure_java:run_java_commands(true, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


measure(TimeFun) ->
  Family = #family{initial = json_gen:empty(), grow = fun json_gen:grow_random/1},
  Axes = #axes{size = fun measure_size/1,
               time = TimeFun,
               repeat = 2,
               measurements = [fun json_gen:max_depth/1]
              },
  ClassPaths = [ "../libs/gson/gson-2.3.1.jar" ],
  {Time, _} = timer:tc(measure_java, measure_java, [1,  50, Family, Axes, ClassPaths]),
  Time / 1000000.


measure_encode() ->
  measure(fun time_encode/1).

measure_decode() ->
  measure(fun time_decode/1).

