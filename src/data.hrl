-type unique_id() :: integer().
-type account_number() :: integer().
-type money() :: number().

-record(account,
    {account_number :: account_number(),
     amount :: money(), 
     given_name :: binary(),
     surname :: binary()}).
% ######################################################################
-record(transfer, 
    {id :: unique_id(), 
     timestamp :: erlang:timestamp(), 
     from_account_number :: account_number(),
     to_account_number :: account_number(),
     amount :: money()}).
% ####### Events
% 

-record(account_event,
    {id :: unique_id(),
     eventType :: account_created,
     account_number :: account_number(),
     givenName :: string(),
     surname :: string()}).
    
-record(transfer_event,{
    source :: transfer_service,
    eventId :: integer(),
    amount :: number(),
    accountIdSender :: integer(),
    accountIdReceiver :: integer(),
    timestamp :: erlang:timestamp()}).

-record(get_account_events_since,{
    since :: non_neg_integer(),
    receiver_pid :: pid()
}).
-record(get_transfer_events_since,{
    since :: non_neg_integer(),
    receiver_pid :: pid()
}).

-type event() :: #account_event{} | #transfer_event{}.
