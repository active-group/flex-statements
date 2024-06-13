# Erlbank Statements Service

Erlbank Futuristic System 

## Interfaces

These are our interfaces.

### Account Creation Notification

Statements Service processes Account Creation Notifications (ACNs).

| field name        | data type   | description                        |
|-------------------|-------------|------------------------------------|
| `account_number`  | `number()`  | Account number of Erlbank customer |
| `given_name`      | `binary()`  | Given name of Erlbank customer     |
| `surname`         | `binary()`  | Surname of Erlbank customer        |
| `amount`          | `number()`  | Initial account balance            |

### Transaction Success Notification

Statements Service processes Transaction Success Notification (TSNs).

| field name             | data type     | description                            |
|------------------------|---------------|----------------------------------------|
| `transaction_id`       | `number()`    | Transaction ID (unique)                |
| `from_account_number`  | `number()`    | Account number of transaction sender   |
| `to_account_number`    | `number()`    | Account number of transaction receiver |
| `amount`               | `number()`    | Amount of money transferred            |
| `timestamp()`          | `timestamp()` | Timestamp of the transaction           |

## Build

```
$ rebar3 compile
```


## Run locally using rebar shell

The service can be run locally including a REPL using

### Shell Befehle für Messaging
- Create Account: gen_server:call(<0.263.0>, {account_created,1,"Peter","Lustig",2000}). 
  -     gen_server:call(<PID>, {Function, AccountNr, Name, Surname, Amount})
- Create Transaction: gen_server:call(<0.263.0>, {transaction_succeeded,1,1,2,1000,erlang:timestamp()}). 
  -     gen_server:call(<PID>, {Function, TransactionID, Sender, Receiver, Amount, Timestamp})

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8002/
