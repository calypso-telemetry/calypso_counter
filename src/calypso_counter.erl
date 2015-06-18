-module(calypso_counter).
-author("Sergey Loguntsov").

%% API
-export([
  start/1, increment/3, get/2, delete/2, clear/1
]).

start(Name) ->
  calypso_counter_sup:add_child(Name).

increment(Name, Key, Step) ->
  calypso_counter_server:increment(Name, Key, Step).

get(Name, Key) ->
  calypso_counter_server:get(Name, Key).

clear(Name) ->
  calypso_counter_server:clear(Name).

delete(Name, Key) ->
  calypso_counter_server:delete(Name, Key).
