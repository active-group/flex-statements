# test Account Service

Service to create a statements.


## Build

```
$ rebar3 compile
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

The web-frontend is served at http://localhost:8002/statements


## Run locally using docker

This project comes with a docker container. It is built using

```
docker build . -t statements
```

in the root directory of the project. To run the docker container call

 ```
 docker run -t -i -p 8002:8002 -e "RELX_REPLACE_OS_VARS=true" -e "NODE_NAME=any_name" statements
 ```

 Running with docker we are able to configure the node name of the erlang node
 using the `NODE_NAME` env var. To do so, relx must be informed that the
 vm.args file contains env vars via `RELX_REPLACE_OS_VARS`.

 If the docker container is up and running, the web-frontend can be found at
 http://localhost:8002/statements


## Testing

rebar3 & eunit are used for testing. To test the service use

```
rebar3 eunit
```

To test it within the docker container use

```
docker run statements test
```


## Release

A release can be built using

```
rebar3 release
```


## Push-Service

Wir nehmen Events an.
* unter `account_created` erwarten wir ein Event nach der Definition aus
https://github.com/active-group/flex-accounts/blob/2023-09/README.md

* unter `transfer_created` erwarten wir ein Event nach der Definition aus
https://github.com/active-group/flex-transfers/blob/main/README.md