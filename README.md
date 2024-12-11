# Erlbank Statements

Erlbank Statements SCS

## Build

```
$ rebar3 compile
```

## Check

You can run the dialyzer via:

```
$ rebar3 dialyzer
```

## Test

You can run the tests in the `tests/` directory via:

```
$ rebar3 eunit
```

## Run locally using rebar shell

The service can be run locally including a REPL using

```
$ rebar3 shell
```

You can set a short name via:

```
$ rebar3 shell --sname=statements
```

The web-frontend is served at http://localhost:8124/
