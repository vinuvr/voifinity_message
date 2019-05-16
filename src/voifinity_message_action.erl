
-module(voifinity_message_action).
-export([init/0
	,store/1
	,recv/2
	,messages/2
	,on_delivered/2
	,publish/1
	,notification_for_all/3
	,push/3
  ,push_determination/5
	]).

-record(undeliveredmsg,{messageid
                       , clientid
                       , topic
                       , message
                       , accountid
                       , field1
                       , field2
                       , field3
                       , field4
                       , field5
                       }).

-record(userinformation,{topic
                        ,faviourategroup
                        ,last_seen
                        ,field1
                        ,field2
                        ,field3
                        ,field4
                        ,field5
                        }).

-record(pushnotification,{deviceid
                         , topic
                         , devicetype
                         , field1
                         , field2
                         , field3
                         , field4
                         , field5
                         }).

-record(storemessage,{messageid
                     , topic
                     , datetime
                     , message
                     , accountid
                     , from
                     , groupid
                     , status
                     , field1
                     , field2
                     , field3
                     , field4
                     , field5
                     }).

-record(group,{groupid
              , memberslist
              , groupadmin
              , owner
              , notificationrestriction
              , groupname
              , field1
              , field2
              , field3
              , field4
              , field5
              }).

-define(groupinstruction, [<<"group_name_change">>
	                       , <<"@_message">>
	                       , <<"off_notify_with_smart_notify">>
	                       , <<"make_admin">>,<<"group_leave_member">>
	                       , <<"group_create">>
	                       , <<"group_delete">>
	                       , <<"group_add_member">>
	                       , <<"group_delete_member">>
	                       , <<"group_message">>
	                        ]).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(undeliveredmsg
  	                 , [{disc_copies
  	                   , [node()]}
  	                   , {attributes, record_info(fields, undeliveredmsg)}
  	                   ,{index, [clientid, topic, message, accountid, field1, field2, field3, field4, field5]}
  	                  ]),
  
  mnesia:create_table(userinformation
  	                 , [{disc_copies
  	                   , [node()]}
  	                   , {attributes, record_info(fields, userinformation)}
  	                   , {index, [faviourategroup, last_seen, field1, field2, field3, field4, field5]}
  	                  ]),

  mnesia:create_table(pushnotification
  	                 , [{disc_copies
  	                   , [node()]}
  	                   , {attributes, record_info(fields, pushnotification)}
  	                   ,{index, [topic, devicetype, field1, field2, field3, field4, field5]}
  	                  ]),

  mnesia:create_table(storemessage
  	                 , [{disc_copies
  	                   , [node()]}
  	                   , {attributes, record_info(fields, storemessage)}
  	                   ,{index, [topic, datetime, message, accountid, from, groupid, field1, field2, field3, field4, field5]}
  	                  ]),

  mnesia:create_table(group
  	                 , [{disc_copies
  	                   , [node()]}
  	                   , {attributes, record_info(fields, group)}
  	                   ,{index,[memberslist, groupadmin, owner, notificationrestriction, groupname, field1, field2, field3,
                       field4, field5]}
  	                  ]).

% when publish hook called
from(Message) ->
  proplists:get_value(<<"from">>, Message).

sender(Message) ->
  proplists:get_value(<<"sender">>, Message).

topic(Message) ->
  proplists:get_value(<<"topic">>, Message).

category(Message) ->
  proplists:get_value(<<"category">>, Message).

messageid(Message) ->
  proplists:get_value(<<"message_id">>, Message).

clientid(Message) ->
  proplists:get_value(<<"clinetId">>, Message).  

message_type(Message) ->
  proplists:get_value(<<"message_type">>, Message).

datetime(Message) ->
  proplists:get_value(<<"datetime">>, Message).

accountid(Message) ->
  proplists:get_value(<<"account_id">>, Message).

groupid(Message) ->
  proplists:get_value(<<"group_id">>, Message).

groupname(Message) ->
  proplists:get_value(<<"group_name">>,Message).

deviceid(Message) ->
  proplists:get_value(<<"device_id">>, Message).

devicetype(Message) ->
  proplists:get_value(<<"device_type">>, Message).

future_admin(Message) ->
  proplists:get_value(<<"future_admin">>, Message).

self_message(Message) ->
  proplists:get_value(<<"self">>, Message).

group_newname(Message) ->
  proplists:get_value(<<"new_group_name">>, Message).

smart_notification_state(Message) ->
  proplists:get_value(<<"smart_notification">>, Message).

scheduled_status(Message) -> 
  proplists:get_value(<<"scheduledstatus">>, Message).

scheduledmsg_proceed(Message) ->
  proplists:get_value(<<"msgscheduled">>, Message).


scheduledtime(Message) 
  proplists:get_value(<<"scheduledtime">>, Message).
            
aftertime(Message) ->
  binary_to_integer(scheduledtime(Message)) - erlang:system_time(seconds).

adding_group_members(Message) ->
  proplists:get_value(<<"adding_group_members">>, Message) 
            -- current_members(Message).
group_members(Message) ->
  proplists:get_value(<<"group_members">>, Message).

deleting_group_members(Message) ->
  proplists:get_value(<<"deleting_member">>, Message).

final_members_in_group(Message, Deleting_members) ->
  current_members(Message) -- [Deleting_members].

notification_required_members(Message) ->
  final_members_in_group(Message) -- [from(Message)].

members(Message) ->
  try  hd(mnesia:dirty_read(group, groupid(Message))) of
    _ ->
      element(3, hd(mnesia:dirty_read(group, groupid(Message)))) -- [from(Message)] 
  catch
    _:_ -> []
  end.

present_notify_restriction(Message) ->
  try hd(mnesia:dirty_read(group, groupid(Message))) of
    _ ->
      element(6, hd(mnesia:dirty_read(group, groupid(Message))))
  catch
    _:_ ->
      []
  end. 

current_members(Message) ->
  try hd(mnesia:dirty_read(group, groupid(Message))) of
    _ ->
      element(3, hd(mnesia:dirty_read(group, groupid(Message))))
  catch
    _:_ ->
     'ok'
  end.

final_admin_in_group(Message) ->
  try hd(mnesia:dirty_read(group, groupid(Message))) of
    _  ->
      element(4,hd(mnesia:dirty_read(group, groupid(Message)))) 
      -- [deleting_group_members(Message)]
  catch 
    _:_  ->
      ok
  end.

final_members_in_group(Message) ->
  case current_members(Message) of
    ok ->
      [] ; 
    _ ->
    current_members(Message) ++ adding_group_members(Message)
  end.
  
present_admins(Message)  ->
  try hd(mnesia:dirty_read(group, groupid(Message))) of
    _ ->
       element(6, hd(mnesia:dirty_read(group, groupid(Message))))
  catch 
    _:_ ->
      []
  end.

final_admin_in_group(Message, From) ->
  element(4,hd(mnesia:dirty_read(group, groupid(Message)))) -- [From].

is_mute_on(Message) ->
  lists:member({from(Message),on}, present_notify_restriction(Message)).

is_mute_off(Message) ->
  lists:member({from(Message),off}, present_notify_restriction(Message)).

is_total_members_within_limit(Message) ->
  lists:member(length(final_members_in_group(Message)), lists:seq(1,25)).

is_self_deleting(Message) ->
  lists:member(from(Message), [deleting_group_members(Message)]). 

is_mute_off_present_for_lefting_member(Message) ->
  lists:member({from(Message), off}, present_notify_restriction(Message)).  

is_mute_off_present_for_deleting_member(Message) ->
  lists:member({deleting_group_members(Message), off}, present_notify_restriction(Message)).

is_mute_on_present_for_lefting_member(Message) ->
  lists:member({from(Message), on}, present_notify_restriction(Message)).

is_message_id_present_in_undeliveredmsg(Message) ->
  lists:member(messageid(Message), mnesia:dirty_all_keys(undeliveredmsg)).
            
is_mute_on_present_for_deleting_member(Message) ->
  lists:member({deleting_group_members(Message), on}, present_notify_restriction(Message)).

is_messagetype_present_in_grouptypes(Message) ->
  lists:member(message_type(Message), ?groupinstruction).

is_mute_off_present(Topic, GroupId) ->
  NotificationRestrictedMembers = element(6, hd(mnesia:dirty_read(group, GroupId))),
  lists:member({Topic,off}, NotificationRestrictedMembers).

is_mute_on_present(Topic, GroupId) ->
  NotificationRestrictedMembers = element(6, hd(mnesia:dirty_read(group, GroupId))),
  lists:member({Topic,on}, NotificationRestrictedMembers).

is_admin(Message) ->
  try  hd(mnesia:dirty_read(group, groupid(Message))) of
   _ ->
    lists:member(from(Message), element(4, hd(mnesia:dirty_read(group, groupid(Message)))))
  catch
    _:_ -> false
  end.

db_user_message(Message, PublishMsg) ->
  Data = #storemessage{messageid = messageid(Message)
                       , topic = topic(Message)
                       , datetime = voifinity_message_utils:gregorian_days(datetime(Message))
                       , message = PublishMsg
                       , accountid = accountid(Message)
                       , from = from(Message)
                       },
  mnesia:dirty_write(storemessage, Data).

db_group_message(Message, PublishMsg) ->
  Data = #storemessage{messageid = messageid(Message)
                       , topic = topic(Message)
                       , datetime = voifinity_message_utils:gregorian_days(datetime(Message))
                       , message = PublishMsg
                       , accountid = accountid(Message)
                       , from = from(Message)
                       , groupid = groupid(Message)
                       },
  mnesia:dirty_write(storemessage, Data). 

db_group_create(Message) ->
  Data = #group{groupid = groupid(Message)
                , memberslist = group_members(Message)
                , groupadmin = [from(Message)]
                , owner = from(Message)
                , notificationrestriction = []
                , groupname = groupname(Message)
                },
  mnesia:dirty_write(group, Data).

db_group_update(Message, Deleting_members, NewNotificationRestriction) ->
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  mnesia:dirty_write(Group#group{groupadmin = final_admin_in_group(Message)
              	                 , memberslist = final_members_in_group(Message, Deleting_members)
              	                 , notificationrestriction = NewNotificationRestriction}
              	                 ).

db_group_update(Message, Deleting_members) ->
  [Group] = mnesia:dirty_read(group,groupid(Message)),
  mnesia:dirty_write(Group#group{memberslist = final_members_in_group(Message, Deleting_members)}).

db_group_update_for_leaving(Message, State) ->
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  NewAdmin = hd(members(Message)),
  mnesia:dirty_write(Group#group{memberslist = members(Message)
              	                 , groupadmin = [NewAdmin]
              	                 , notificationrestriction = present_notify_restriction(Message) -- State
              	                 });

db_group_update_for_leaving(Message, []) ->
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  NewAdmin = hd(members(Message)),
  mnesia:dirty_write(Group#group{memberslist = members(Message)
                                , groupadmin = [NewAdmin]
               	                }).

db_group_update_for_leaving(Message, no_admin_change, State) ->
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  mnesia:dirty_write(Group#group{memberslist = members(Message)
              	                 , notificationrestriction = present_notify_restriction(Message) -- State
              	                 }).

db_group_update_for_leaving(Message) ->
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  mnesia:dirty_write(Group#group{memberslist = members(Message)}).

db_notification_message(Message, PublishMsg) ->
  Data = #storemessage{messageid = messageid(Message)
                       , topic = topic(Message)
                       , datetime = voifinity_message_utils:gregorian_days(datetime(Message))
                       , message = PublishMsg
                       },
  mnesia:dirty_write(storemessage, Data).

db_user_token(Message) ->
  Data = #pushnotification{deviceid = deviceid(Message)
                           , topic = topic(Message)
                           , devicetype = devicetype(Message)
                           },
  mnesia:dirty_write(pushnotification, Data).

db_notification_restriction(Message, {From, on}) ->
  NewState = present_notify_restriction(Message) -- [{From, off}],
  case Group = mnesia:dirty_read(group, groupid(Message)) of
    [] ->
      ok;
    _ ->
      Groups = hd(Group),
      mnesia:dirty_write(Groups#group{notificationrestriction = NewState ++ [{From, on}]})
  end;

db_notification_restriction(Message, {From, off}) ->
  NewState = present_notify_restriction(Message) -- [{From, on}],
  [Group] = mnesia:dirty_read(group,groupid(Message)),
  mnesia:dirty_write(Group#group{notificationrestriction = NewState ++ [{From, off}]}).

db_unmute_notification(Message) ->
  NotificationRestriction = element(6, hd(mnesGroupa:dirty_read(group, groupid(Message)))),
  State1 = present_notify_restriction(Message) -- [{from(Message), on}],
  State2 = State1 -- [{from(Message), off}],
  [Group] = mnesia:dirty_read(group, groupid(Message)),
  mnesia:dirty_write(Group#group{notificationrestriction = State2}).

db_undeliveredmsg_store(Message, PublishMsg) ->
  UndeliveredMsg = #undeliveredmsg{messageid = message_type(Message)
                                   , clientid = topic(Message)
                                   , topic = topic(Message)
                                   , message = PublishMsg
                                   , accountid = accountid(Message)
                                   },
  mnesia:dirty_write(undeliveredmsg, UndeliveredMsg).

db_undeliveredmsg_delete(Message) ->
  mnesia:dirty_delete(undeliveredmsg, messageid(Message)).

db_storemessage_update(Message, Status) ->
  [StoreMsg] = mnesia:dirty_read(storemessage, messageid(Message)),
  mnesia:dirty_write(StoreMsg#storemessage{status = Status }).

db_delete_message(Message) ->
  MessageId = proplists:get_value(<<"deleteId">>, Message),
  %MnesiaStoredMsg = element(5, StoreMsg),
  StoreMsg =mnesia:dirty_read(storemessage, MessageId),
  case {StoreMsg, self(Message)}
    {[], true} ->
      ok;
    {_, undefined}  ->
      StoreMsg1 = hd(StoreMsg),
      MnesiaStoredMsg = element(5, hd(StoreMsg)),
      StoredMsg = element(2, hd(jsx:decode(element(8, MnesiaStoredMsg)))),
      Message1 = lists:keyreplace(<<"payload">>, 1, StoredMsg, {<<"payload">>, <<"This message was deleted">>}),
      FinalMessage = {[{<<"data">>, Message1}]},
      EncodedFinalMsg = jsx:encode(element(1, FinalMessage)),
      ChangedMsg = erlang:setelement(8, MnesiaStoredMsg, EncodedFinalMsg),
      mnesia:dirty_write(StoreMsg1#storemessage{message = ChangedMsg}),
      voifinity_message_utils:self_message(From, clientid(Message), Message);
    {_, true} ->
      ok
  end.

db_edit_message(Message) ->
  MessageId = proplists:get_value(<<"editId">>, Message),
  EditedMsg = proplists:get_value(<<"payload">>, Message),
  StoreMsg = mnesia:dirty_read(storemessage, MessageId)
  case {StoreMsg, self(Message)} of
    {[], true} ->
      ok;
    {_, undefined}  ->
      StoreMsg1 = hd(StoreMsg),
      MnesiaStoredMsg = element(5, hd(StoreMsg)),
      StoredMsg = element(2, hd(jsx:decode(element(8, MnesiaStoredMsg)))),
      Message1 = lists:keyreplace(<<"payload">>, 1, StoredMsg, {<<"payload">>, EditedMsg}),
      FinalMessage = {[{<<"data">>, Message1}]},
      EncodedFinalMsg = jsx:encode(element(1, FinalMessage)),
      ChangedMsg = erlang:setelement(8, MnesiaStoredMsg, EncodedFinalMsg),
      mnesia:dirty_write(StoreMsg1#storemessage{message = ChangedMsg}),
      voifinity_message_utils:self_message(from(Message), clientid(Message), Message),
      io:format("edit message worked successfully\n");
    {_, true} ->
      ok
  end.

% when publish hook is called
publish(Message) ->
  case MsgCheck = element(8, Message) of
    <<"Connection Closed abnormally..!">> ->
      io:format("\nmqtt client closed successfully...!\n");
    _ ->
      Args = element(2, hd(jsx:decode(element(8, Message)))),

      case  proplists:get_value(<<"message_type">>, Args) of
        <<"server_status">> -> 
          ok;

        <<"server_delivered_status">> -> 
          ok;

        <<"group_name_change_notification">> ->
          ok;

        <<"self_message">> ->
          ok;

        <<"group_create_notification">> ->  
          db_notification_message(Args, Message);

        <<"group_added_notification">> ->  
          db_notification_message(Args, Message);

        <<"group_remove_notificaton">> ->  
          db_notification_message(Args, Message);

        <<"group_deleted_notification">> -> 
          db_notification_message(Args, Message);

        <<"group_left_notification">> -> 
          db_notification_message(Args, Message);   

        <<"group_admin_notification">> ->  
           db_notification_message(Args, Message);
        
        <<"group_notification">> -> 
          ok;

        <<"special_notification">> -> 
          db_notification_message(Args, Message);

        <<"user_add_token">>  ->  
          db_user_token(Args);

        <<"user_message">>  ->  
          case {self(Args), scheduled_status(Args), scheduledmsg_proceed(Args)} of
            {undefined, undefined, _} ->
              db_user_message(Args, Message),
              voifinity_message_utils:self_message(from(Args), clientid(Args), Args),
              voifinity_message_utils:server_status(from(Args), messageid(Args), Args);
            
            {undefined, <<"true">>, undefined} ->
              db_user_message(Args, Message),
              voifinity_message_utils:self_message(from(Args), clientid(Args), Args),
              %voifinity_message_utils:server_status(from(Args), messageid(Args), Args),
              voifinity_message_utils:do_scheduledmsg_publish(aftertime(Args), from(Args), clientid(Args), Args);
           
            {undefined, <<"true">>, <<"success">>} ->
              ok;

            {true, _, _} ->
              ok;

            {_,  _, _ } ->
              ok
          
          end;

        <<"@_message">> ->  
          voifinity_message_utils:special_message(messageid(Args), members(Args), Args); 

        <<"unmute_notification">> ->  
          db_unmute_notification(Args);

        <<"group_message">> ->  
          db_group_message(Args, Message),
          voifinity_message_utils:convert(messageid(Args), members(Args), Args);

        <<"make_admin">>  ->  
          case lists:member(from(Args), present_admins(Args)) of
            true ->
              [Group]=mnesia:dirty_read(group, groupid(Args)),  
              mnesia:dirty_write(Group#group{groupadmin = 
              	                             present_admins(Args) ++ [future_admin(Args)]}
              	                 ),
              voifinity_message_utils:admin(messageid(Args), members(Args), Args);
            false ->
              io:format("you are not admin\n")
          end;
       
        <<"off_notify_with_smart_notify">> -> 
          case {smart_notification_state(Args), is_mute_on(Args), is_mute_off(Args)} of
            {<<"On">>, false, false} ->
              db_notification_restriction(Args, {from(Args), on});
           
            {<<"On">>, true, false} ->
               ok;
           
            {<<"On">>, false, true} ->
               db_notification_restriction(Args, {from(Args), on});  
           
            {<<"Off">>, false, false} ->
              db_notification_restriction(Args, {from(Args), off});
           
            {<<"Off">>, true, false} ->
              db_notification_restriction(Args, {from(Args), off });
           
            {<<"Off">>, false, true}  ->
              ok
         end;
     
        <<"add_faviourate_group">> -> 
          case  CurrentFavGroup = mnesia:dirty_read(userinformation, from(Args)) of
            [] ->
              User = #userinformation{topic = from(Args)
                                     , faviourategroup = [{groupid(Args), groupname(Args)}]
                                      },
              mnesia:dirty_write(userinformation,User);
           
            [{Groupid, Groupname}] ->
              ok;
           
            _  ->
              FinalFavGroup = element(3, hd(CurrentFavGroup)) ++ 
                              [{groupid(Args), groupname(Args)}],

              User = #userinformation{topic = from(Args),faviourategroup = FinalFavGroup},
              mnesia:dirty_write(userinformation,User)
          end;

        <<"info_message">> -> 
          case category(Args) of
            <<"remove_message">> ->
              db_delete_message(Args);
            <<"edit_message">> ->
               db_edit_message(Args);
             _ ->
               ok
          end;
           
        <<"group_create">>     ->  
    
          Restmember = group_members(Args) -- [from(Args)],
          db_group_create(Args),
          voifinity_message_utils:create_notification(messageid(Args), Restmember, Args),
          voifinity_message_utils:added_notification(messageid(Args), Restmember, Args);

        <<"group_name_change">>  -> 
          case Group =  mnesia:dirty_read(group, groupid(Args)) of
            [] ->
              ok;
            _ ->
              Groups = hd(Group),
              mnesia:dirty_write( Groups#group{groupname = group_newname(Args)}),
              voifinity_message_utils:name_change_notification(messageid(Args)
          	                                               , members(Args)
          	                                               , Args)
          end;
        
      
        <<"group_delete">>     -> 
           case is_admin(Args) of
            true ->
              voifinity_message_utils:deleted_notification(messageid(Args)
              	                                           , members(Args)
              	                                           , Args),
              mnesia:dirty_delete(group, groupid(Args));
            false ->
              ok
          end;

        <<"group_add_member">> -> 
          case {is_admin(Args)
          	    , is_total_members_within_limit(Args)} of
            {true, true } ->

              [Group] = mnesia:dirty_read(group, groupid(Args)),
              mnesia:dirty_write(Group#group{memberslist = final_members_in_group(Args)}),
              voifinity_message_utils:added_notification(messageid(Args)
                                                        , notification_required_members(Args)
                                                        , Args
                                                        );
            {false, _} ->
              ok;
            {_, _} ->
              ok
          end;

        <<"group_delete_member">> -> 
          case {is_admin(Args)
          	    , is_self_deleting(Args)
          	    , is_mute_on_present_for_deleting_member(Args)
          	    , is_mute_off_present_for_deleting_member(Args) 
          	    } of
           
            {false, _, _, _}  ->
              ok;
            {true, false, true, _} ->
              NewNotificationRestriction = present_notify_restriction(Args) 
                                           -- [{deleting_group_members(Args), on}],

              %NewAdmins = element(4,hd(mnesia:dirty_read(group,GroupId))) -- [DeletingMemberInGroup],
              voifinity_message_utils:deleted_notification(messageid(Args), members(Args), Args),
              db_group_update(Args, deleting_group_members(Args), NewNotificationRestriction);
           
            {true, false, false, false} ->
              %[Group]=mnesia:dirty_read(group,GroupId),
              %mnesia:dirty_write(Group#group{memberslist=FinalMembersInGroup}),
              db_group_update(Args, deleting_group_members(Args)),
              voifinity_message_utils:deleted_notification(messageid(Args), members(Args), Args);
            {true,false,false,true} ->
              NewNotificationRestriction = present_notify_restriction(Args) 
                                          -- [{deleting_group_members(Args), off}],
              %NewAdmins = element(4,hd(mnesia:dirty_read(group,GroupId))) -- [DeletingMemberInGroup],
              voifinity_message_utils:deleted_notification(messageid(Args)
              	                                          , members(Args)
              	                                          , Args),
              db_group_update(Args, deleting_group_members(Args), NewNotificationRestriction);
     
            {true,true,_,_} ->
              ok
          end;

        <<"group_leave_member">>  -> 
          case {length(members(Args))
          	    , final_admin_in_group(Args)
          	    , is_mute_on_present_for_lefting_member
          	    , is_mute_off_present_for_lefting_member} of
            {0, _, _, _} ->
              mnesia:dirty_delete(group, groupid(Args));
            {0, ok, _ ,_ } ->
              ok;
            {_, [], true, _} ->
              db_group_update_for_leaving(Args, [{from(Args), on}]),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args),
              io:format("admin has changed\n");
            {_, [], _, true} ->
              db_group_update_for_leaving(Args, [{from(Args), off}]),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args),
              io:format("admin has changed\n");
            {_, [], false, false} ->
              db_group_update_for_leaving(Args,[]),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args);
            {_, _, true, _} ->
              db_group_update_for_leaving(Args, no_admin_change, [{from(Args), on}]),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args), 
              io:format("no change in admin\n");
            {_, _, _, true} ->
              db_group_update_for_leaving(Args, no_admin_change, [{from(Args), off}]),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args),
              io:format("no change in admin\n");
            {_, _, false, false} ->
              db_group_update_for_leaving(Args),
              voifinity_message_utils:left_notification(messageid(Args), members(Args), Args)
          end;

        _  ->
          ok;
      end
  end.

%%% when drop hook called      
store(Inputmessage) ->
  WillMsg = element(7, Inputmessage),
  case WillMsg of
    <<"WillMsg">> ->
      'ok';
    _ ->
      Message = element(8, Inputmessage),
      Proplist = element(2, hd(jsx:decode(Message))),
      case {is_message_id_present_in_undeliveredmsg(Proplist)
            , is_messagetype_present_in_grouptypes(Proplist)
            , message_type(Proplist)
           }  of
        {false, false, <<"user_message">>} ->
          %db_undeliveredmsg_store(Proplist, Message),
      
         % [StoredMsg]=mnesia:dirty_read(storemessage,MessageId),
         % mnesia:dirty_write(StoredMsg#storemessage{status= <<"sent">> });
           ok;
   %%%      push_determination(Group_id,MessageType,Topic);
        {false, false, <<"server_delivered_status">>}  ->
          %db_undeliveredmsg_store(Proplist, Message),  
           ok;
        {false, false, <<"server_status">>} ->
          %db_undeliveredmsg_store(Args)
           ok;
        {false, false, <<"info_message">>} ->
          ok;
        {false, false, _} ->
          %UndeliveredMsg =#undeliveredmsg{messageid = MessageId
          %                  ,clientid = Topic
          %                  ,topic = Topic
          %                  ,message = Message
          %                  ,accountid = AccountId
          %                  },
          %mnesia:dirty_write(undeliveredmsg,UndeliveredMsg);
          ok;
        {false, true,_} ->
          io:format("   condition 1\n");
        {true, true,_}  ->
          io:format("condition 2\n");
        {true, false,_} ->
           io:format(" condition3\n");
        {_, _ ,_} ->
          ok
      end
  end.
    
%% when subscription hook called
recv(_,Topic) -> 
  io:format("\n\n\n received subscribtion hook ==================~p\n",[Topic]),
  Data     =mnesia:dirty_index_read(undeliveredmsg, Topic, topic),
  case Data of
    [] ->
      ok;
    _ ->  
      messages(Topic, Data)
  end.

messages(_, []) ->
  ok;
messages(Topic, [H|T]) ->
  Message = element(5,H),
  Data = emqx_message:make(Topic, Message),
  emqx:publish(Data),
  messages(Topic, T).

%% ack hook is called
on_delivered(Clientid, Message)-> 
  MsgCheck = element(8, Message),
  case MsgCheck of
    <<"Connection Closed abnormally..!">> ->
      'ok';
    _ ->
      Proplist = element(2, hd(jsx:decode(element(8, Message)))),
      Topic = element(7, Message),
      %io:format("Messageid ~p acked and  delivered to topic ~p from ~p\n",[MessageId,Topic,From]),
      case {is_message_id_present_in_undeliveredmsg(Proplist), message_type(Proplist), self(Proplist)}  of
        %{true, <<"user_message">>, undefined} ->
        % voifinity_message_utils:server_status_delivered(from(Proplist), messageid(Proplist), Proplist),
        %  db_undeliveredmsg_delete(Proplist),
        %  db_storemessage_update(Proplist, <<"delivered">>);
      
        {_, <<"user_message">>, undefined} ->
          voifinity_message_utils:server_status_delivered(from(Proplist), messageid(Proplist), Proplist),
          db_storemessage_update(Proplist, <<"delivered">>);
        
        {true, _, undefined} ->
          %db_undeliveredmsg_delete(Proplist),
          ok;
        {_, _, _}  ->
          ok
      end
  end.

%% on message drop is called
push_determination(GroupId, MessageType, Topic, TypedMsg, Sender) ->
  case GroupId of
    undefined ->
      push(Topic, TypedMsg, Sender);
    _    ->
     
         case {MessageType
              , is_mute_off_present(Topic, GroupId)
              , is_mute_on_present(Topic, GroupId) } of

          {_, true, _} ->
            ok;

          {<<"special_notification">>, _, true} ->
            push(Topic, TypedMsg, Sender);                   
          {<<"special_notification">>, false, false} ->
            push(Topic, TypedMsg, Sender);
          {_, _, _} ->
            push(Topic, TypedMsg, Sender)
        end
    end.

push(Topic, TypedMsg, Sender) ->
  Devices = mnesia:dirty_index_read(pushnotification, Topic, topic),
  io:format("pushing\n"),   
  case Devices of
    [] ->
      ok;
     _ ->      
      notification_for_all(Devices, TypedMsg, Sender)
   end.

notification_for_all([], TypedMsg, Sender) ->
  ok;

notification_for_all([H|T], TypedMsg, Sender) ->
  DeviceType      =element(4, H),
  DeviceId        =element(2, H),
  case DeviceType == <<"android">> of
    true -> 
      io:format("pushing message to the device id\n"),
      fcm:push(push,DeviceId,[{<<"notification">>
      	                      ,[{<<"body">>, TypedMsg}
      	                      ,{<<"sound">>, <<"default">>}
      	                      ,{<<"title">>, Sender }
      	                       ]}
      	                     ]),
      notification_for_all(T, TypedMsg, Sender);
    false ->
      notification_for_all(T, TypedMsg, Sender)
  end.


















        
         


