-module(voifinity_message_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  voifinity_message_clearing:start(),
  fcm:start(push,"AAAAD0HDJSY:APA91bFrOZt5SEf6DNaTD84IULOPpppYoETJRS7ANMUAABpm_bpPzz48Uf2BFF3gbW_1M1HYV0nwSxVY38bYo-_naUcxI47VNGPmsc4kuw6fSJN91MJR0IZa3lmiSyymQsU2GtqAWrn4").

init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

