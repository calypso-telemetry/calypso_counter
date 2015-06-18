-module(calypso_counter_app).
-author("Sergey Loguntsov").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  calypso_counter_sup:start_link().

-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

