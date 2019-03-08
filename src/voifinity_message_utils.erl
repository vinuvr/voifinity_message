-module(voifinity_message_utils). 
-export([convert/3
	    ,admin/3
	    ,special_message/3
	    ,create_notification/3
	    ,added_notification/3
	    ,name_change_notification/3
	    ,deleted_notification/3
	    ,left_notification/3
	    ,server_status/3
	    ,server_status_delivered/3
	    ,gregorian_days/1
	    ]).
convert(_,[],_) ->
  ok;
convert(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"group_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}),
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  convert(MessageId,T,Message).
admin(_,[],_) ->
  ok;
admin(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"admin_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  admin(MessageId,T,Message).
special_message(_,[],_) ->
  ok;
special_message(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"special_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  special_message(MessageId,T,Message).
create_notification(_,[],_) ->
  ok;
create_notification(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"create_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  create_notification(MessageId,T,Message).
added_notification(_,[],_) ->
  ok;
added_notification(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++
                 binary_to_list(H) ++ binary_to_list(<<"added">>)), 
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"added_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  added_notification(MessageId,T,Message).
name_change_notification(_,[],_) ->
  ok;
name_change_notification(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"name_change_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  name_change_notification(MessageId,T,Message). 
deleted_notification(_,[],_) ->
  ok;
deleted_notification(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"deleted_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish), 
  deleted_notification(MessageId,T,Message).
left_notification(_,[],_) ->
  ok;
left_notification(MessageId,[H|T],Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(H)),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"left_notification">>}),
  Message3 = lists:keyreplace(<<"clientId">>,1,Message2,{<<"clientId">>,H}), 
  FinalMessage = {[{<<"data">>,Message3}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(H,EncodedFinalMsg),
  emqx:publish(Publish),
  left_notification(MessageId,T,Message).
server_status(From,MessageId,Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(<<"-sent">>)),
  TimeSort = erlang:system_time(milli_seconds),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"server_status">>}),
  Message3 = lists:keyreplace(<<"timesort">>,1,Message2,{<<"timesort">>,TimeSort}),
  Message4 = Message3 ++ [{<<"status">>,<<"sent">>}
                          ,{<<"class">>,<<"messageStatus">>}
                         ],
  FinalMessage = {[{<<"data">>,Message4}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish =emqx_message:make(From,EncodedFinalMsg),
  emqx:publish(Publish).
server_status_delivered(From,MessageId,Message) ->
  NewMessageId = list_to_binary(binary_to_list(MessageId) ++ binary_to_list(<<"-delivered">>)),
  TimeSort = erlang:system_time(milli_seconds),
  {{Year,Month,Day},{Hour,Minute,Second}} =calendar:now_to_universal_time(erlang:timestamp()),
  Time = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",[Year,Month, Day, Hour, Minute, Second]),
  Message1 = lists:keyreplace(<<"message_id">>,1,Message,{<<"message_id">>,NewMessageId}),
  Message2 = lists:keyreplace(<<"message_type">>,1,Message1,{<<"message_type">>,<<"server_delivered_status">>}),
  Message3 = lists:keyreplace(<<"datetime">>,1,Message2,{<<"datetime">>,list_to_binary(Time)}),
  Message4 = lists:keyreplace(<<"timesort">>,1,Message3,{<<"timesort">>,TimeSort}),
  Message5 = Message4 ++ [{<<"status">>,<<"delivered">>}
                          ,{<<"class">>,<<"messageStatus">>}
                         ],
  FinalMessage = {[{<<"data">>,Message5}]},
  EncodedFinalMsg = jsx:encode(element(1,FinalMessage)),
  Publish = emqx_message:make(From,EncodedFinalMsg),
  emqx:publish(Publish).

gregorian_days(A) ->
  A1=binary_to_list(A),
  {Y,M,D} = { list_to_integer(lists:sublist(binary_to_list(A),1,4))
             ,list_to_integer(lists:sublist(binary_to_list(A),6,2))
             ,list_to_integer(lists:sublist(binary_to_list(A),9,2))
             },
  Out=integer_to_binary(calendar:date_to_gregorian_days({Y,M,D})).



