-module(calypso_counter_server).
-author("Sergey Loguntsov").

-behaviour(gen_server).

%% API
-export([start_link/1, clear/1, increment/3, get/2, delete/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  ets
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(Name :: atom()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) when is_atom(Name) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ Name ], []).

clear(Name) ->
  gen_server:call(Name, clear).

increment(Name, Key, Step) when is_atom(Name), is_integer(Step) ->
  try
    ets:update_counter(Name, Key, { 2, Step })
  catch
    error:badarg ->
      ets:insert(Name, { Key, Step }),
      Step
  end.

get(Name, Key) when is_atom(Name) ->
  case ets:lookup(Name, Key) of
    [{ _, V}] -> V;
    [] -> undefined
  end.

delete(Name, Key) when is_atom(Name) ->
  ets:delete(Name, Key).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ Name ]) ->
  Ets = ets:new(Name, [ named_table, set, public, { write_concurrency, true}, { read_concurrency, true } ]),
  {ok, #state{
    ets = Ets
  }}.

-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(clear, _From, State) ->
  ets:delete_all_objects(ets(State)),
  { reply, ok, State };

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ets(State) -> State#state.ets.