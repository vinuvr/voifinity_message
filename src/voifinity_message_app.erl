-module(voifinity_message_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = voifinity_message_sup:start_link(),
   voifinity_message_action:init(),
   voifinity_message:load(application:get_all_env()),
   {ok, Sup}.

stop(_State) ->
  voifinity_message:unload().
