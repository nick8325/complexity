
-module(example_json_simple).


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
  measure_java:run_java_commands(true, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


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
  measure_java:run_java_commands(true, 50, lists:flatten(SetupCommands), lists:flatten(RunCommands)).


measure(TimeFun) ->
  Family = #family{initial = json_gen:empty(), grow = fun json_gen:grow_all/1},
  Axes = #axes{size = fun measure_size/1,
               time = TimeFun,
               repeat = 2},
  {Time, _} = timer:tc(measure_java, measure_java, [1,  100, Family, Axes]),
  Time / 1000000.


measure_encode() ->
  measure(fun time_encode/1).

measure_decode() ->
  measure(fun time_decode/1).

