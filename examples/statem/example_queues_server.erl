
-module(example_queues_server).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include("../measure.hrl").


%-define(TEST_MODULE, queues_server_eqc).

-record(sequence, { state, commands = [] }).


patch_commands(M, V, State, Cmds) ->
  patch_commands(M, V, State, [], Cmds).

%% Check the preconditions and compute the new next states for
%% the provided commands.
patch_commands(_M, _V, _State, Acc, []) -> lists:reverse(Acc);
%patch_commands(M, V, State, Acc, [Cmd | Cmds]) ->
%  case catch {ok, M:adapt(State, element(3, Cmd))} of
%    {ok, PatchedCmd} ->
%      io:format(">>> Patched command: ~p~n", [PatchedCmd]),
%      PostState = ?TEST_MODULE:next_state(State, V, PatchedCmd), 
%      patch_commands(M, V + 1, PostState, [PatchedCmd | Acc], Cmds);
%    E ->
%      io:format(">>> patch_commands FAILED: ~p~n", [E]),
%      failed
%  end.
patch_commands(M, V, State, Acc, [Cmd | Cmds]) ->
  case M:precondition(State, element(3, Cmd)) of
    true ->
%      io:format(">>> patch_commands SUCCEEDED:~n ~p~n in ~p~n after ~p~n", [Cmd, State, lists:reverse(Acc)]),
      PostState = M:next_state(State, V, element(3, Cmd)),
      patch_commands(M, V + 1, PostState, [Cmd | Acc], Cmds);
    false ->
%      io:format(">>> patch_commands FAILED:~n ~p~n in ~p~n after ~p~n", [Cmd, State, lists:reverse(Acc)]),
      failed
  end.


commands_gen(M, V, State) -> 
  ?LET(Cmds, vector(10, M:command(State)),
    [ {set, {var, V}, Cmd} || Cmd <- lists:usort(Cmds) ]).

%% Inserts a new command in the command sequence between the
%% prefix and the rest of the commands.
commands_gen_insert(M, V, Prefix, State, Cmds) ->
  ?LET(NewCmds, commands_gen(M, V, State),
    lists:concat(
      [ begin
          NewPostState = M:next_state(State, V + 1, element(3, NewCmd)),
          case patch_commands(M, V + 2, NewPostState, Cmds) of
            failed -> [];
            PatchedCmds -> [Prefix ++ [NewCmd] ++ PatchedCmds]
          end
        end || NewCmd <- NewCmds ])).

%% Skips the first command in the command sequence and calls
%% commands_gen recursively to insert a command at a later
%% point in the list.
commands_gen_skip(M, V, Prefix, State, CmdHead, CmdTail) ->
  PostState = M:next_state(State, V, element(3, CmdHead)),
  commands_gen(M, V + 1, Prefix ++ [CmdHead], PostState, CmdTail).  


commands_gen(M, V, Prefix, State, []) ->
  ?LET(Cmds, commands_gen(M, V, State),
    [ Prefix ++ [Cmd] || Cmd <- Cmds ]);
commands_gen(M, V, Prefix, State, Cmds = [CmdHead | CmdTail]) ->
  ?LET(InsertCmds, commands_gen_insert(M, V, Prefix, State, Cmds),
    ?LET(SkipCmds, commands_gen_skip(M, V, Prefix, State, CmdHead, CmdTail),
      InsertCmds ++ SkipCmds)). 


%% The grow function used when measuring the server.
sequences_gen(M, S) ->
  V = (length(S#sequence.commands) + 1) * 3,
  ?LET(Cmdss, commands_gen(M, V, [], S#sequence.state, S#sequence.commands),
       [ S#sequence{commands = Cmds} || Cmds <- Cmdss]).


%% The size function used when measuring the server.
sequence_length(S) ->
  length(S#sequence.commands).


%% The run functions used when measuring the server.
run_sequence(M, S) ->
  Result = eqc_statem:run_commands(M, S#sequence.commands),
  case element(3, Result) of
    ok -> ok;
    X -> erlang:exit({invalid_sequence_result, X})
  end.

measure_queues_server() ->
  M = queues_server_eqc,
  State = M:initial_state(), 
  measure(1, 15,
          #family{initial = #sequence{state = State, commands = []}, grow = fun(S) -> sequences_gen(M, S) end},
          #axes{size = fun sequence_length/1,
                time = time1(fun(S) -> run_sequence(M, S) end)}).

