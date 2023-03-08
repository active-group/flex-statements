# Bank-Statemnts Service

Service to display bank-statement.

This service contains multiple micro-services:

- date_formatter: A service for locale based date formatting
- price_formatter: A service for locale based number formatting (e.g. 1000-separators)
- exchange_service: A service for calculating exchange rates based on current ECB rates which are constantly updated


## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The application consumes an account feed and an transfer feed. To know, where
these feeds are, the node identifiers have to be passed to the service, using an
env variable. The application can be run in an development environment including
a REPL using the following command:

```
$ TRANSFER_NODE=node_name1 ACCOUNT_NODE=node_name2 rebar3 shell
```

When the nodes `node_name1` and `node_name2` are not reachable, there will be an
error, but the service will continue running. To get data into the database
anyways, call `dev:setup().`.

The web-frontend is served on http://localhost:8000/bank-statement


## Run locally using docker

This project comes with a docker container. It is built using 

```
docker build . -t bank-statements
```

in the root directory of the project. To run the docker container call
 
 ```
 docker run -p 8000:8000 -e "RELX_REPLACE_OS_VARS=true" -e "NODE_NAME=any_name" -e "ACCOUNT_NODE=some_name" -e "TRANSFER_NODE=some_name" bank-statements
 ```
 
 Running with docker we are able to configure the node name of the erlang node
 using the `NODE_NAME` env var. To do so, relx must be informed that the 
 vm.args file contains env vars via `RELX_REPLACE_OS_VARS`.
 
 If the docker container is up and running, the web-frontend can be found at
 http://localhost:8000/bank-statements


## Testing

rebar3 & eunit are used for testing. To test the service use

```
rebar3 eunit
```

To test it within the docker container use

```
docker run bank-statements test
```


## Release

A release can be built using 

```
rebar3 release
```

