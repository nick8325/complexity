-module(example_http).

-compile(export_all).


-include("measure.hrl").
-include_lib("eqc/include/eqc.hrl").


-define(MAXARGS, 10).


%% Grow the value of one of the arguments.
grow_value(Args) ->
  grow_value([], Args).

grow_value(_Prefix, []) -> [];
grow_value(Prefix, [ Arg = {Name, Value} | Args]) ->
  Value2 = Value ++ [ elements(lists:seq($a, $z)) ], 
  [ Prefix ++ [ {Name, Value2} ] ++ Args ] ++
  grow_value(Prefix ++ [Arg], Args).


%% Grow the arguments by adding a new argument or growing
%% the value of one of the arguments.
grow(Args) ->
  ArgCount = length(Args),
  [ Args ++ [ { io_lib:format("arg~p", [ArgCount]), "" }]  || ArgCount < ?MAXARGS ] ++
  grow_value(Args).
  
make_args_string(Args) ->
  ArgString = string:join([ io_lib:format("~s=~s", [Name, Arg]) || {Name, Arg} <- Args ], "&").

measure_size(Args) ->
  length(make_args_string(Args)).

make_call(Args) ->
  Request = "http://httpbin.org/get?" ++ make_args_string(Args),
  {ok, Result} = httpc:request(Request),
  {{_Version, 200, _Reason}, _Headers, Body} = Result,
  Body.

measure_time(Args) ->
  {Time, _} = timer:tc(?MODULE, make_call, [Args]),
  Time.


measure_http() ->
  catch inets:start(),
  measure(1, 100,
          #family{initial = [], grow = fun grow/1},
          #axes{size = fun measure_size/1, time = fun measure_time/1, repeat = 2}).

