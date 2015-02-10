
-module(queues_server_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-compile(export_all).


-record(state, { started = false, elements = [] }).


initial_state() ->
  #state{}.



%%----- Command: start_link 

start_link_pre(S) ->
  not S#state.started.

start_link_args(_S) ->
  [].

start_link() ->
  catch queues_server:stop(),
  queues_server:start_link().

start_link_next(S, _Result, []) ->
  S#state{ started = true }.


%%----- Command: in 
in_pre(S) ->
  S#state.started.

in_args(_S) ->
%  [nat()].
  [elements([a, b])].

in(X) ->
  queues_server:in(X).

in_next(S, _Result, [X]) ->
  S#state{ elements = S#state.elements ++ [X] }.


%%----- Command: in_r 
in_r_pre(S) ->
  S#state.started.

in_r_args(_S) ->
  [nat()].

in_r(X) ->
  queues_server:in_r(X).

in_r_next(S, _Result, [X]) ->
  S#state{ elements = [ X | S#state.elements ] }.


%%----- Command: out 
out_pre(S) ->
  S#state.started andalso
  S#state.elements /= [].

out_args(_S) ->
  [].

out() ->
  queues_server:out().

out_next(S, _Result, []) ->
  S#state{ elements = tl(S#state.elements) }.

out_post(S, _Args, X) ->
  eq(X, hd(S#state.elements)). 


%%----- Command: out_r 
out_r_pre(S) ->
  S#state.started andalso
  S#state.elements /= [].

out_r_args(_S) ->
  [].

out_r() ->
  queues_server:out_r().

out_r_next(S, _Result, []) ->
  S#state{ elements = lists:reverse(tl(lists:reverse(S#state.elements))) }.

out_r_post(S, _Args, X) ->
  eq(X, hd(lists:reverse(S#state.elements))). 


%% ----- Properties
prop_queues_server() ->
  ?SETUP(
     fun() ->
       fun() -> ok end
     end,
     ?FORALL(Cmds, commands(?MODULE),
       begin
         io:format("Cmds = ~p\n", [Cmds]),
         {H, S, Res} = run_commands(?MODULE, Cmds),
         measure('Commands', length(Cmds),
           aggregate(command_names(Cmds),
             pretty_commands(?MODULE, Cmds, {H, S, Res}, Res == ok)))
       end)).


%the_time() ->
%    {reductions, X} = process_info(self(), reductions),
%    X.
%
%time(Fun) ->
%    erlang:garbage_collect(),
%    A = the_time(),
%    Fun(),
%    B = the_time(),
%    B - A.
%
%complexity() ->
%  complexity([], initial_state()).
%
%complexity(Cmds, State) ->
%  io:format("Cmds = ~p~n", [Cmds]),
%  io:format(" state = ~p~n", [State]),
%  Cmd = eqc_gen:pick(?MODULE:command(State)),
%  %io:format(" *** Cmd = ~p~n", [Cmd]),
%  SetCmd = {set, {var, length(Cmds) + 1}, Cmd},
%  State2 = ?MODULE:next_state(State, length(Cmds) + 1, Cmd),
%  Cmds2 = Cmds ++ [ SetCmd ],
%  Time = time(fun() -> eqc_statem:run_commands(?MODULE, Cmds2) end),
%  io:format(">>> Time = ~p~n", [Time]),
%  case length(Cmds2) > 10 of
%    true -> ok;
%    _ ->
%      complexity(Cmds2, State2)
%  end.


