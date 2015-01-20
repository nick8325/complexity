
-module(queues_server).


-behaviour(gen_server).


-compile(export_all).


-define(SERVER, ?MODULE).


-record(state, { queue }).


%% Server functions

init(_) ->
  {ok, #state{ queue = queues:new() }}.


handle_call(size, _From, S) ->
  {reply, queues:size(S#state.queue), S};

handle_call({in, X}, _From, S) ->
  S2 = S#state{ queue = queues:in(X, S#state.queue) },
  {reply, ok, S2};

handle_call({in_r, X}, _From, S) ->
  S2 = S#state{ queue = queues:in_r(X, S#state.queue) },
  {reply, ok, S2};

handle_call(out, _From, S) ->
  {X, Q} = queues:out(S#state.queue),
  S2 = S#state{ queue = Q },
  {reply, X, S2};

handle_call(out_r, _From, S) ->
  {X, Q} = queues:out_r(S#state.queue),
  S2 = S#state{ queue = Q },
  {reply, X, S2};

handle_call(terminate, _From, S) ->
  unregister(?SERVER),
  {stop, normal, ok, S}.


handle_cast(_Request, S) ->
  {noreply, S}.

handle_info(_Request, S) ->
  {noreply, S}.

terminate(_Reason, _S) ->
  ok.

code_change(_OldVersion, S, _Extra) ->
  {ok, S}.


%% Starts the server.
start_link() ->
  {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Stops the server.
stop() ->
  gen_server:call(?SERVER, terminate).


%% Returns the size of the queue.
size() ->
  gen_server:call(?SERVER, size).


%% Adds an element to the end of the queue.
in(X) ->
  gen_server:call(?SERVER, {in, X}).


%% Adds an element to the front of the queue.
in_r(X) ->
  gen_server:call(?SERVER, {in_r, X}).


%% Removes an element from the front of the queue.
out() ->
  gen_server:call(?SERVER, out).


%% Removes an element from the end of the queue.
out_r() ->
  gen_server:call(?SERVER, out_r).

 
