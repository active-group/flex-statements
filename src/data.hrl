-type money() :: number().
-type unique_id() :: integer().
-type account_number() :: integer().

-record(transfer, 
    {id :: unique_id(),
     timestamp :: date_formatter:timestamp(), 
     from_acc_nr :: account_number(), 
     to_acc_nr :: account_number(),
     amount :: money()}).
-record(person, 
    {first_name :: binary(), 
     last_name :: binary()}).
-record(account, 
  {person :: #person{},
   transfers :: list(#transfer{}), 
   amount :: money()}).

-record(event, 
    {index :: non_neg_integer(), 
     type :: atom(),
     content :: term()}).
