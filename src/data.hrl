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


-record(accountEvent,
    {id :: unique_id(),
     eventType :: account_created,
     account_number :: account_number(),
     givenName :: string(),
     surname :: string()}).
    
-record(transferEvent,{
    source :: transfer_service,
    eventId :: integer(),
    accountIdSender :: integer(),
    accountIdReceiver :: integer(),
    timestamp :: erlang:timestamp()}).