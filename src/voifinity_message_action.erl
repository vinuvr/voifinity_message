
-module(voifinity_message_action).
-export([init/0
	,store/1
	,recv/2
	,messages/2
	,on_delivered/2
	,publish/1
	,notification_for_all/1
	,push/1
        ,push_determination/3
	]).
-record(undeliveredmsg,{messageid,clientid,topic,message,accountid,f1,f2,f3,f4,f5}).
-record(userinformation,{topic,faviourategroup,last_seen,f1,f2,f3,f4,f5}).
-record(pushnotification,{deviceid,topic,devicetype,f1,f2,f3,f4,f5}).
-record(storemessage,{messageid,topic,datetime,message,accountid,from,groupid,status,f1,f2,f3,f4,f5}).
-record(group,{groupid,memberslist,groupadmin,owner,notificationrestriction,groupname,f1,f2,f3,f4,f5}).
-define(groupinstruction,[<<"group_name_change">>
	                     ,<<"@_message">>
	                     ,<<"off_notify_with_smart_notify">>
	                     ,<<"make_admin">>,<<"group_leave_member">>
	                     ,<<"group_create">>
	                     ,<<"group_delete">>
	                     ,<<"group_add_member">>
	                     ,<<"group_delete_member">>
	                     ,<<"group_message">>
	                     ]).

init() ->
  %
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(undeliveredmsg
  	                 ,[{disc_copies
  	                   ,[node()]}
  	                   ,{attributes,record_info(fields,undeliveredmsg)}
  	                   ,{index,[clientid,topic,message,accountid,f1,f2,f3,f4,f5]}
  	                  ]),
  mnesia:create_table(userinformation
  	                 ,[{disc_copies
  	                   ,[node()]}
  	                   ,{attributes,record_info(fields,userinformation)}
  	                   ,{index,[faviourategroup,last_seen,f1,f2,f3,f4,f5]}
  	                  ]),
  mnesia:create_table(pushnotification
  	                 ,[{disc_copies
  	                   ,[node()]}
  	                   ,{attributes,record_info(fields,pushnotification)}
  	                   ,{index,[topic,devicetype,f1,f2,f3,f4,f5]}
  	                  ]),
  mnesia:create_table(storemessage
  	                 ,[{disc_copies
  	                   ,[node()]}
  	                   ,{attributes,record_info(fields,storemessage)}
  	                   ,{index,[topic,datetime,message,accountid,from,groupid,f1,f2,f3,f4,f5]}
  	                  ]),

  mnesia:create_table(group
  	                 ,[{disc_copies
  	                   ,[node()]}
  	                   ,{attributes,record_info(fields,group)}
  	                   ,{index,[memberslist,groupadmin,owner,notificationrestriction,groupname,f1,f2,f3,f4,f5]}
  	                  ]).

% when publish hook called
publish(Message) ->
  DecodedMessage= element(2,hd(jsx:decode(element(8,Message)))),
  case  proplists:get_value(<<"message_type">>,DecodedMessage) of
    <<"user_message">>  ->  
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      From = proplists:get_value(<<"from">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      AccountId = proplists:get_value(<<"account_id">>,DecodedMessage),
      Data = #storemessage{messageid = MessageId
                          ,topic = Topic
                          ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                          ,message = Message
                          ,accountid = AccountId
                          ,from = From
                          },
      mnesia:dirty_write(storemessage,Data); 
    <<"group_message">> ->  
      From =proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      AccountId = proplists:get_value(<<"account_id">>,DecodedMessage),
      Members = element(3,hd(mnesia:dirty_read(group,GroupId))) --[From],
      Data = #storemessage{messageid = MessageId
                          ,topic = GroupId
                          ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                          ,message = Message
                          ,accountid = AccountId
                          ,from = From
                          ,groupid = GroupId
                          },
      mnesia:dirty_write(storemessage,Data),
      voifinity_message_utils:convert(MessageId,Members,DecodedMessage);
    <<"info_message">> -> 
      ok;
    <<"server_status">> -> 
      ok;
    <<"server_delivered_status">> -> 
      ok;
    <<"create_notification">> ->  
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         },
      mnesia:dirty_write(storemessage,Data);
    <<"added_notification">> ->  
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                          ,topic = Topic
                          ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                          ,message = Message
                          },
      mnesia:dirty_write(storemessage,Data);
    <<"remove_notificaton">> ->  
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         },
      mnesia:dirty_write(storemessage,Data);
    <<"deleted_notification">> -> 
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         },
      mnesia:dirty_write(storemessage,Data);
    <<"left_notification">> -> 
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         },
      mnesia:dirty_write(storemessage,Data);   
    <<"admin_notification">> ->  
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         },
      mnesia:dirty_write(storemessage,Data);
    <<"group_notification">> -> 
      ok; 
    <<"user_add_token">>  ->  
      DeviceId = proplists:get_value(<<"device_id">>,DecodedMessage),
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      DeviceType = proplists:get_value(<<"device_type">>,DecodedMessage),
      Data =#pushnotification{deviceid = DeviceId
                             ,topic = Topic
                             ,devicetype = DeviceType
                             },
      mnesia:dirty_write(pushnotification,Data);
    <<"make_admin">>  ->  
      From = proplists:get_value(<<"from">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      FutureAdmin = proplists:get_value(<<"future_admin">>,DecodedMessage),
      PresentAdmins = element(4,hd(mnesia:dirty_read(group,GroupId))),
      Members = element(3,hd(mnesia:dirty_read(group,GroupId))) -- [From],
      case lists:member(From,PresentAdmins) of
        true ->
          [Group]=mnesia:dirty_read(group,GroupId),  
          mnesia:dirty_write(Group#group{groupadmin = PresentAdmins ++ [FutureAdmin]}),
          voifinity_message_utils:admin(MessageId,Members,DecodedMessage);
        false ->
          io:format("you are not admin\n")
     end;
    <<"off_notify_with_smart_notify">> -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      SmartNotificationState = proplists:get_value(<<"smart_notification">>,DecodedMessage),
      PresentNotifyRestriction = element(6,hd(mnesia:dirty_read(group,GroupId))),
      Available1 = lists:member({From,on},PresentNotifyRestriction),
      Available2 = lists:member({From,off},PresentNotifyRestriction),
      case {SmartNotificationState,Available1,Available2} of
        {<<"On">>,false,false} ->
          Data = [{From,on}],
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{notificationrestriction = PresentNotifyRestriction ++ Data});
        {<<"On">>,true,false} ->
           ok;
        {<<"On">>,false,true} ->
          NewState = PresentNotifyRestriction -- [{From,off}],
          Data = [{From,on}],
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{notificationrestriction = NewState ++ Data});  
        {<<"Off">>,false,false} ->
          Data = [{From,off}],
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{notificationrestriction = PresentNotifyRestriction ++ Data});
        {<<"Off">>,true,false} ->
          NewState=PresentNotifyRestriction -- [{From,on}],
          Data = [{From,off}],
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{notificationrestriction = NewState ++ Data});
        {<<"Off">>,false,true}  ->
          ok
     end;
    <<"special_notification">> -> 
      Topic = proplists:get_value(<<"topic">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message=Message
                         },
      mnesia:dirty_write(storemessage,Data);
    <<"add_faviourate_group">> -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      GroupName = proplists:get_value(<<"group_name">>,DecodedMessage),
      case  CurrentFavGroup = mnesia:dirty_read(userinformation,From) of
        [] ->
          User =#userinformation{topic = From,faviourategroup = [{GroupId,GroupName}]},
          mnesia:dirty_write(userinformation,User);
        _  ->
          FinalFavGroup = element(3,hd(CurrentFavGroup)) ++ [{GroupId,GroupName}],
          User =#userinformation{topic = From,faviourategroup = FinalFavGroup},
          mnesia:dirty_write(userinformation,User)
      end;
    <<"@_message">> ->  
      From = proplists:get_value(<<"from">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      Members = element(3,hd(mnesia:dirty_read(group,GroupId))) -- [From],
      voifinity_message_utils:special_message(MessageId,Members,DecodedMessage);    
    <<"mute_notification">> ->  
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      NotificationRestriction = element(6,hd(mnesGroupa:dirty_read(group,GroupId))),
      State1 = NotificationRestriction -- [{From,on}],
      State2 = State1 -- [{From,off}],
      [Group] = mnesia:dirty_read(group,GroupId),
      mnesia:dirty_write(Group#group{notificationrestriction = State2});
    <<"group_create">>     ->  
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      GroupMembers = proplists:get_value(<<"group_members">>,DecodedMessage),
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupName = proplists:get_value(<<"group_name">>,DecodedMessage),
      Data =#group{groupid = GroupId
                  ,memberslist = GroupMembers
                  ,groupadmin = [From]
                  ,owner = From
                  ,notificationrestriction = []
                  ,groupname = GroupName
                  },
      mnesia:dirty_write(group,Data),
      Restmember = GroupMembers -- [From],
      voifinity_message_utils:create_notification(MessageId,Restmember,DecodedMessage),
      voifinity_message_utils:added_notification(MessageId,Restmember,DecodedMessage);
    <<"group_name_change">>  -> 
      io:format("dfdlkjflkkk\n"),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      From = proplists:get_value(<<"from">>,DecodedMessage),
      AccountId = proplists:get_value(<<"account_id">>,DecodedMessage),
      Datetime = proplists:get_value(<<"datetime">>,DecodedMessage),
      Newname = proplists:get_value(<<"new_group_name">>,DecodedMessage),
      Topic = proplists:get_value(<<"clientId">>,DecodedMessage),
      Members = element(3,hd(mnesia:dirty_read(group,GroupId))) -- [From],
      [Group] = mnesia:dirty_read(group,GroupId),
      mnesia:dirty_write(Group#group{groupname = Newname}),
      voifinity_message_utils:name_change_notification(MessageId,Members,DecodedMessage),
      Data =#storemessage{messageid = MessageId
                         ,topic = Topic
                         ,datetime = voifinity_message_utils:gregorian_days(Datetime)
                         ,message = Message
                         ,accountid = AccountId
                         ,groupid = GroupId
                         ,from = From
                         },
       mnesia:dirty_write(storemessage,Data);
    <<"name_change_notification">> -> 
      ok;
    <<"group_delete">>     -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      case lists:member(From,element(4,hd(mnesia:dirty_read(group,GroupId)))) of
        true ->
          Members = element(3,hd(mnesia:dirty_read(group,GroupId))) -- [From],
          voifinity_message_utils:deleted_notification(MessageId,Members,DecodedMessage),
          mnesia:dirty_delete(group,GroupId);
        false ->
          ok
      end;
    <<"group_add_member">> -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      CurrentMembersInGroup = element(3,hd(mnesia:dirty_read(group,GroupId))),
      AddingGroupMembers = proplists:get_value(<<"adding_group_members">>,DecodedMessage) 
                                                  -- CurrentMembersInGroup,
      FinalMembers = CurrentMembersInGroup ++ AddingGroupMembers,
      FinalMembersStrength = length(FinalMembers),
      case {lists:member(From,element(4,hd(mnesia:dirty_read(group,GroupId))))
      	    ,lists:member(FinalMembersStrength,lists:seq(1,25))} of
        {true,true } ->
          NotificationRequiredMembers = FinalMembers --[From],
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{memberslist = FinalMembers}),
          voifinity_message_utils:added_notification(MessageId
                                                    ,NotificationRequiredMembers
                                                    ,DecodedMessage
                                                    );
        {_,_} ->
          ok
      end;
    <<"group_delete_member">> -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      CurrentMembersInGroup = element(3,hd(mnesia:dirty_read(group,GroupId))),
      DeletingMemberInGroup = proplists:get_value(<<"deleting_member">>,DecodedMessage),
      FinalMembersInGroup = CurrentMembersInGroup -- [DeletingMemberInGroup],
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      PresentNotifyRestriction = element(6,hd(mnesia:dirty_read(group,GroupId))),
      AdminInGroup = lists:member(From ,element(4,hd(mnesia:dirty_read(group,GroupId)))),
      SelfDeleting = lists:member(From,[DeletingMemberInGroup]),
      DeletingNotification1 = lists:member({DeletingMemberInGroup,on},PresentNotifyRestriction),
      DeletingNotification2 = lists:member({DeletingMemberInGroup,off},PresentNotifyRestriction),
      Members = CurrentMembersInGroup -- [From],
      case {AdminInGroup,SelfDeleting,DeletingNotification1,DeletingNotification2} of
        {false,_,_,_}  ->
          ok;
        {true,false,true,_} ->
          NewNotificationRestriction = PresentNotifyRestriction -- [{DeletingMemberInGroup,on}],
          NewAdmins = element(4,hd(mnesia:dirty_read(group,GroupId))) -- [DeletingMemberInGroup],
          voifinity_message_utils:deleted_notification(MessageId,Members,DecodedMessage),
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{groupadmin = NewAdmins
          	                 ,memberslist = FinalMembersInGroup
          	                 ,notificationrestriction = NewNotificationRestriction
          	                 });
        {true,false,false,false} ->
          [Group]=mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{memberslist=FinalMembersInGroup}),
          voifinity_message_utils:deleted_notification(MessageId,Members,DecodedMessage);
        {true,false,false,true} ->
          NewNotificationRestriction = PresentNotifyRestriction--[{DeletingMemberInGroup,off}],
          NewAdmins = element(4,hd(mnesia:dirty_read(group,GroupId))) -- [DeletingMemberInGroup],
          voifinity_message_utils:deleted_notification(MessageId,Members,DecodedMessage),
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{groupadmin = NewAdmins
          	                         ,memberslist = FinalMembersInGroup
          	                         ,notificationrestriction = NewNotificationRestriction
          	                         });
        {true,true,_,_} ->
          ok
      end;
    <<"group_leave_member">>  -> 
      From = proplists:get_value(<<"from">>,DecodedMessage),
      GroupId = proplists:get_value(<<"group_id">>,DecodedMessage),
      MessageId = proplists:get_value(<<"message_id">>,DecodedMessage),
      Members = element(3,hd(mnesia:dirty_read(group,GroupId))) -- [From],
      Admin = element(4,hd(mnesia:dirty_read(group,GroupId))),
      FinalAdmin = Admin -- [From],
      PresentNotifyRestriction = element(6,hd(mnesia:dirty_read(group,GroupId))),
      Available1 = lists:member({From,on},PresentNotifyRestriction),
      Available2 = lists:member({From,off},PresentNotifyRestriction),
      case {length(Members),FinalAdmin,Available1,Available2} of
        {0,_,_,_} ->
          mnesia:dirty_delete(group,GroupId);
        {_,[],true,_} ->
          [Group]=mnesia:dirty_read(group,GroupId),
          NewAdmin=hd(Members),
          mnesia:dirty_write(Group#group{memberslist = Members
          	                 ,groupadmin = [NewAdmin]
          	                 ,notificationrestriction = PresentNotifyRestriction -- [{From,on}]
          	                 }),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage),
          io:format("admin has changed\n");
        {_,[],_,true} ->
          [Group] = mnesia:dirty_read(group,GroupId),
          NewAdmin = hd(Members),
          mnesia:dirty_write(Group#group{memberslist = Members
          	                 ,groupadmin = [NewAdmin]
          	                 ,notificationrestriction = PresentNotifyRestriction--[{From,off}]
          	                 }),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage),
          io:format("admin has changed\n");
        {_,[],false,false} ->
          [Group] = mnesia:dirty_read(group,GroupId),
          NewAdmin = hd(Members),
          mnesia:dirty_write(Group#group{memberslist = Members
          	                ,groupadmin = [NewAdmin]
          	                }),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage);
        {_,_,true,_} ->
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{memberslist = Members
          	                 ,notificationrestriction = PresentNotifyRestriction--[{From,on}]
          	                 }),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage), 
          io:format("no change in admin\n");
        {_,_,_,true} ->
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{memberslist = Members
          	                 ,notificationrestriction = PresentNotifyRestriction--[{From,off}]
          	                 }),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage),
          io:format("no change in admin\n");
        {_,_,false,false} ->
          [Group] = mnesia:dirty_read(group,GroupId),
          mnesia:dirty_write(Group#group{memberslist = Members}),
          voifinity_message_utils:left_notification(MessageId,Members,DecodedMessage)
      end
  end.

%%% when drop hook called      
store(Inputmessage) ->
  Message = element(8,Inputmessage),
  Proplist = element(2,hd(jsx:decode(Message))),
  MessageId = proplists:get_value(<<"message_id">>,Proplist),
  MessageType = proplists:get_value(<<"message_type">>,Proplist),
  From = proplists:get_value(<<"from">>,Proplist),
  Topic = proplists:get_value(<<"clientId">>,Proplist),
  AccountId = proplists:get_value(<<"account_id">>,Proplist),
  case {lists:member(MessageId,mnesia:dirty_all_keys(undeliveredmsg))
        ,lists:member(MessageType,?groupinstruction)
        ,MessageType
       }  of
    {false,false,<<"user_message">>} ->
      UndeliveredMsg =#undeliveredmsg{messageid = MessageId
                                      ,clientid = Topic
                                      ,topic = Topic
                                      ,message = Message
                                      ,accountid = AccountId
                                      },
      mnesia:dirty_write(undeliveredmsg,UndeliveredMsg),
      [StoredMsg]=mnesia:dirty_read(storemessage,MessageId),
      mnesia:dirty_write(StoredMsg#storemessage{status= <<"sent">> }),
      voifinity_message_utils:server_status(From,MessageId,Proplist);
%      push_determination(Group_id,MessageType,Topic);
    {false,false,<<"server_delivered_status">>}  ->
       UndeliveredMsg =#undeliveredmsg{messageid = MessageId
                         ,clientid = Topic
                         ,topic = Topic
                         ,message = Message
                         ,accountid = AccountId
                         },
       mnesia:dirty_write (undeliveredmsg,UndeliveredMsg);
    {false,false,<<"server_status">>} ->
       UndeliveredMsg =#undeliveredmsg{messageid = MessageId
                          ,clientid = Topic
                          ,topic = Topic
                          ,message = Message
                          ,accountid = AccountId
                          },
       mnesia:dirty_write(undeliveredmsg,UndeliveredMsg);
    {false,false,<<"info_message">>} ->
      ok;
    {false,false,_} ->
      UndeliveredMsg =#undeliveredmsg{messageid = MessageId
                        ,clientid = Topic
                        ,topic = Topic
                        ,message = Message
                        ,accountid = AccountId
                        },
      mnesia:dirty_write(undeliveredmsg,UndeliveredMsg);
    {false,true,_} ->
      io:format("   condition 1\n");
    {true,true,_}  ->
      io:format("condition 2\n");
    {true,false,_} ->
       io:format(" condition3\n")
  end.

%% when subscription hook called
recv(_,Topic) -> 
  io:format("\n\n\n~p\n",[Topic]),
  Data     =mnesia:dirty_index_read(undeliveredmsg,Topic,topic),
  case Data of
    [] ->
      ok;
    _ ->  
      messages(Topic,Data)
  end.

messages(_,[]) ->
  ok;
messages(Topic,[H|T]) ->
  Message = element(5,H),
  Data = emqx_message:make(Topic,Message),
  emqx:publish(Data),
  messages(Topic,T).

%% delivered hook called
on_delivered(_,Message)-> %ClientId
  Proplist = element(2,hd(jsx:decode(element(8,Message)))),
  MessageId = proplists:get_value(<<"message_id">>,Proplist),
  From = proplists:get_value(<<"from">>,Proplist),
  MessageType = proplists:get_value(<<"message_type">>,Proplist),
  Topic = element(7,Message),
  io:format("Message delivered to \n\n~p\n~p\n~p\n",[MessageId,Topic,From]),
  case {lists:member(MessageId,mnesia:dirty_all_keys(undeliveredmsg)),MessageType}  of
    {true,<<"user_message">>} ->
      voifinity_message_utils:server_status_delivered(From,MessageId,Proplist),
      mnesia:dirty_delete(undeliveredmsg,MessageId),
      [StoreMsg] = mnesia:dirty_read(storemessage,MessageId),
      mnesia:dirty_write(StoreMsg#storemessage{ status = <<"delivered">>});
    {false,<<"user_message">>} ->
      voifinity_message_utils:server_status_delivered(From,MessageId,Proplist),
      [StoreMsg] = mnesia:dirty_read(storemessage,MessageId),
      mnesia:dirty_write(StoreMsg#storemessage{ status = <<"delivered">>});
    {true,_} ->
      mnesia:dirty_delete(undeliveredmsg,MessageId);
     % [StoreMsg] = mnesia:dirty_read(storemessage,MessageId),
     % mnesia:dirty_write(StoreMsg#storemessage{ status = <<"delivered">>});
    {_,_}  ->
      ok
  end.
%% on drop 
push_determination(GroupId,MessageType,Topic) ->
  case GroupId of
    undefined ->
      push(Topic);
    _    ->
      NotificationRestrictedMembers = element(6,hd(mnesia:dirty_read(group,GroupId))),
        case {MessageType,lists:member({Topic,off}
             ,NotificationRestrictedMembers)
             ,lists:member({Topic,on}
             ,NotificationRestrictedMembers)} of
          {_,true,_} ->
                 ok;
          {<<"special_notification">>,_,true} ->
                   push(Topic);                   
          {<<"special_notification">>,false,false} ->
             push(Topic);
          {_,_,_} ->
             push(Topic)
        end
    end.

push(Topic) ->
  Devices=mnesia:dirty_index_read(pushnotification,Topic,topic),
  io:format("pushing\n"),   
  case Devices of
    [] ->
      ok;
     _ ->      
      notification_for_all(Devices)
   end.

notification_for_all([]) ->
  ok;
notification_for_all([H|T]) ->
  DeviceType      =element(4,H),
  DeviceId        =element(2,H),
  case DeviceType == <<"android">> of
    true -> 
      io:format("pushing message to the device id\n"),
      fcm:push(push,DeviceId,[{<<"notification">>
      	                      ,[{<<"body">>,<<"New message">>}
      	                      ,{<<"sound">>,<<"default">>}
      	                      ,{<<"title">>,<<"Message">>}
      	                       ]}
      	                     ]),
      notification_for_all(T);
    false ->
      notification_for_all(T)
  end.


%% when Client disconnects 
%% %last_seen(ClientId)  ->
%% %  case mnesia:dirty_read(userinformation,ClientId) of
%% %    [] ->
%% %       UserInformation = #userinformation{topic = ClientId
%% %                                          ,last_seen = erlang:universaltime()
%% %                                          },
%% %       mnesia:dirty_write(userinformation,UserInformation);
%% %    _  ->
%% %      [UserInformation] =mnesia:dirty_read(userinformation,ClientId),
%% %      mnesia:dirty_write(UserInformation#userinformation{last_seen=erlang:universaltime()})
%% %  end.
%% %%% when drop hook called























        
         


