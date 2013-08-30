%% Support for measuring sequences of operations.
-module(commands).
-export([measure/3]).

-record(spec, {initial, size, time, valid, commands, query, amortised}).
-record(test, {query, commands, value, time}).

spec(Module) ->
    #spec{
       initial = Module:initial(),
       size = override2(fun Module:size/2,
                       fun(_, Cmds) -> length(Cmds) end),
       time = override1(fun Module:time/1,
                       fun(_) -> 1 end),
       valid = override3(fun Module:valid/3, fun(_,_,_) -> true end),
       commands = fun Module:commands/2,
       query = override1(fun Module:query/1, fun(_) -> false end),
       amortised = override(fun Module:amortised/0,
                            fun() -> false end)
      }.

override1(Fun1, Fun2) ->
    fun(X) -> override(fun() -> Fun1(X) end,
                       fun() -> Fun2(X) end) end.

override2(Fun1, Fun2) ->
    fun(X, Y) -> override(fun() -> Fun1(X, Y) end,
                          fun() -> Fun2(X, Y) end) end.

override3(Fun1, Fun2) ->
    fun(X, Y, Z) -> override(fun() -> Fun1(X, Y, Z) end,
                             fun() -> Fun2(X, Y, Z) end) end.

override(Fun1, Fun2) ->
    case catch {ok, Fun1()} of
        {ok, Result} -> Result;
        {'EXIT', {function_clause, _}} -> Fun2();
        {'EXIT', {{case_clause, _}, _}} -> Fun2();
        {'EXIT', {undef, _}} -> Fun2()
    end.

%% Can't use eqc.hrl because it imports measure/3,
%% and then we can't define our own measure/3 :(
-define(LET(X, Gen, R), eqc_gen:bind(Gen, fun(X) -> R end)).

measure(Rounds, Points, Module) ->
    Spec = spec(Module),
    Test = #test{query = undefined,
                 commands = [],
                 value = Spec#spec.initial},
    measure:measure(Rounds, Points,
                    fun(X) -> size(Spec, X) end,
                    ?LET(X, gen(Spec, Test), eqc_gen:elements(X)),
                    fun(X) -> gen(Spec, X) end,
                    fun(X) -> time(Spec, X) end).

filter_out(P, Xs) ->
    [ X || X <- Xs, not P(X) ].

gen(Spec = #spec{commands = Commands, query = Query}, Test) ->
    ?LET(Cands, Commands(Test#test.value, Test#test.commands),
    case Query(Test#test.query) orelse Test#test.query == undefined of
        true ->
            ?LET(Cmd, eqc_gen:elements(filter_out(Query, Cands)),
            gen(Spec, Test#test{query = Cmd}));
        false ->
            lists:concat(
            [ test(Spec, Cmds)
            || Cmd <- Cands,
               Cmds <- insert_anywhere(Spec, Cmd, [Test#test.query|Test#test.commands]) ])
    end).

eval(Test) ->
    eqc_symbolic:eval([{value, Test#test.value}], Test#test.query).

splits(Xs) ->
    [ lists:split(N, Xs)
    || N <- lists:seq(0, length(Xs)) ].

insert_anywhere(#spec{query = Query}, X, Xs) ->
    case Query(X) of
        true -> [[X|Xs]];
        false ->
            [ Ys ++ [X] ++ Zs || {Ys, Zs} <- splits(Xs) ]
    end.

test(Spec, [Cmd|Cmds]) ->
    case test_commands(Spec, Cmds) of
        bad -> [];
        {ok, Value, Time} ->
            [#test{query = Cmd,
                   commands = Cmds,
                   value = Value,
                   time = Time}]
    end.

test_commands(Spec, []) ->
    {ok, Spec#spec.initial, 0};
test_commands(Spec = #spec{valid = Valid, time = Time, query = Query}, [Cmd|Cmds]) ->
    case Query(Cmd) of
        true -> bad;
        false ->
            case test_commands(Spec, Cmds) of
                bad -> bad;
                {ok, Value, Time0} ->
                    case true of %Valid(Cmd, Value, Cmds) of
                        false -> bad;
                        true ->
                            Test = #test{query = Cmd, value = Value},
                            case catch {ok, timing:time_and_result(fun() -> eval(Test) end)} of
                                {ok, {Time1, Value1}} ->
                                    {ok, Value1, Time(Cmd) * Time1 + Time0};
                                {'EXIT', _} -> bad
                            end
                    end
            end
    end.

size(#spec{size = Size}, Test) ->
    Size(Test#test.value, Test#test.commands).

time(#spec{amortised = false, time = Time}, Test) ->
    Time(Test#test.query) * timing:time(fun() -> eval(Test) end);
time(#spec{amortised = true, time = Time}, Test) ->
    (Time(Test#test.query) * timing:time(fun() -> eval(Test) end) +
     Test#test.time).
