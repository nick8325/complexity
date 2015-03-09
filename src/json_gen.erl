-module(json_gen).

-include_lib("eqc/include/eqc.hrl").
-include("measure.hrl").

-compile(export_all).



%% Generates a name that doesn't exists in the
%% provided list of names.
name(Names) ->
  ?SUCHTHAT(Name,
            ?LET(Name, non_empty(list(elements(lists:seq($a, $z)))), binary:list_to_bin(Name)),
%            ?LET(Name, vector(3, elements(lists:seq($a, $z))), binary:list_to_bin(Name)),
            not(lists:member(Name, Names))).


%% Generates a field value.
value() ->
  int().  


%% Returns an empty structure.
empty() ->
  [].


%% Returns a list of all paths to the elements
%% (both arrays and fields) in the structure.
paths(Lst) when is_list(Lst) ->
  [[]] ++ lists:concat([ paths([], X) || X <- Lst ]).

paths(Path, {Name, {value, _}}) ->
  [Path ++ [Name]];
paths(Path, {Name, Lst}) when is_list(Lst) ->
  Path2 = Path ++ [Name],
  [Path2] ++ lists:concat([ paths(Path2, X) || X <- Lst ]).


%% Extends the array or fields with the provided path.
extend([], {value, X}) ->
  [];
extend([], Lst) when is_list(Lst) ->
  Names = [ Name || {Name, _} <- Lst ],
  X = {name(Names), {value, value()}}, 
  Lst ++ [X];
extend([ Name | Path], Lst) when is_list(Lst) ->
  [ if Name == XName -> {Name, extend(Path, X)};
       true -> {XName, X}
    end || {XName, X} <- Lst ]. 


%% Returns the maximum depth of the structure.
max_depth({value, _}) -> 0;
max_depth([]) -> 1;
max_depth(Lst) when is_list(Lst) ->
  1 + lists:max([ max_depth(X) || {_, X} <- Lst]).


take(N, Lst) ->
  case N > length(Lst) of
    true -> Lst;
    _ -> {X, _} = lists:split(N, Lst), X 
  end.

 
%% Grows the structure by extending one of its elements.
%% Returns a list of ten random extention points.
grow_random(X) ->
  ?LET(Paths, shuffle(paths(X)),
    [ extend(Path, X) || Path <- take(10, Paths) ]).
%  vector(10,
%    ?LET(Path, elements(paths(X)), extend(Path, X))).

%% Grows the structure by extending one of its elements.
%% Returns a list of all extention points.
grow_all(X) ->
  [ extend(Path, X) || Path <- paths(X) ].
 
