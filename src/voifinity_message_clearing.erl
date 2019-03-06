%%%-------------------------------------------------------------------
%%%%%% @author vinu
%%%%%% @copyright (C) 2018, vinu
%%%%%% @doc
%%%%%%
%%%%%% @end
%%%%%% Created : 2018-12-17 11:30:43
%%%%%%-------------------------------------------------------------------
-module(voifinity_message_clearing).
-behaviour(gen_server).
%% gen_server_callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).
-define(INTERVAL,24*60*60000).
%% API
-export([start/0,stop/0,databaseclearing/0]).
-record(state,{}).
-type state():: #state{}.
%%%===================================================================
%%%%%% API
%%%%%%===================================================================
%%%
%%%%%--------------------------------------------------------------------
%%%%% @doc
%%%%% Starts the server
%%%%%
%%%%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%%%%% @end
%%%%%--------------------------------------------------------------------
start() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).
%%%===================================================================
%%%%%% gen_server callbacks
%%%%%%===================================================================
%%%
%%%%%--------------------------------------------------------------------
%%%%% @private
%%%%% @doc
%%%%% Initializes the server
%%%%%
%%%%% @spec init(Args) -> {ok, State} |
%%%%% {ok, State, Timeout} |
%%%%% ignore |
%%%%% {stop, Reason}
%%%%% @end
%%%%%--------------------------------------------------------------------
init([]) ->
  erlang:send_after(?INTERVAL,self(),trigger),
  {ok,#state{}}.
%%--------------------------------------------------------------------
%%%% @private
%%%%% @doc
%%%%% Handling call messages
%%%%%
%%%%% @spec handle_call(Request, From, State) ->
%%%%% {reply, Reply, State} |
%%%%% {reply, Reply, State, Timeout} |
%%%%% {noreply, State} |
%%%%% {noreply, State, Timeout} |
%%%%% {stop, Reason, Reply, State} |
%%%%% {stop, Reason, State}
%%%%% @end
%%%%%--------------------------------------------------------------------
handle_call(_Request,_From,State) ->
  {noreply,State}.
%%--------------------------------------------------------------------
%%%% @private
%%%%% @doc
%%%%% Handling cast messages
%%%%%
%%%%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%%%% {noreply, State, Timeout} |
%%%%% {stop, Reason, State}
%%%%% @end
%%%%%--------------------------------------------------------------------
handle_cast(stop,State) ->
  {stop,normal,State}.
%%--------------------------------------------------------------------
%%%% @private
%%%%% @doc
%%%%% Handling all non call/cast messages
%%%%%
%%%%% @spec handle_info(Info, State) -> {noreply, State} |
%%%%% {noreply, State, Timeout} |
%%%%% {stop, Reason, State}
%%%%% @end
handle_info(trigger,State) ->
  databaseclearing(),
  erlang:send_after(?INTERVAL,self(),trigger),
  {noreply,State}.
%%--------------------------------------------------------------------
%%%% @private
%%%%% @doc
%%%%% This function is called by a gen_server when it is about to
%%%%% terminate. It should be the opposite of Module:init/1 and do any
%%%%% necessary cleaning up. When it returns, the gen_server terminates
%%%%% with Reason. The return value is ignored.
%%%%%
%%%%% @spec terminate(Reason, State) -> void()
%%%%% @end
%%%%%--------------------------------------------------------------------
terminate(_Reason,State) ->
  {noreplay,State}.
%%--------------------------------------------------------------------
%%%% @private
%%%%% @doc
%%%%% Convert process state when code is changed
%%%%%
%%%%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%%%% @end
code_change(_OldVsn,State,_Extra) ->
  {ok,State}.
%%%===================================================================
%%%%%% Internal functions
%%%%%%===================================================================
stop() ->
  gen_server:cast(?MODULE,stop).

databaseclearing() ->
  {Date,_} = calendar:local_time(),
  DeletingDate = integer_to_binary(calendar:date_to_gregorian_days(Date)-90),  
  case Data = mnesia:dirty_index_read(storemessage,DeletingDate,datetime)  of
    [] -> 
      ok;
     _ ->
      clearingmessages(Data)
   end.

clearingmessages([]) ->
  ok;
clearingmessages([H|T]) ->
    mnesia:dirty_delete(storemessage,element(2,H)),
    %mnesia:dirty_delete(undeliveredmsg,element(2,H)),
    clearingmessages(T).
 



