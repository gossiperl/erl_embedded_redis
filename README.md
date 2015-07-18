# Embedded Redis server for Erlang

Start Redis as an Erlang application. Project inspired by [kstyrc/embedded-redis](https://github.com/kstyrc/embedded-redis).

# Using in your project

## As a `rebar` dependency

    {deps, [
      {erl_embedded_redis, ".*",
        {git, "https://github.com/gossiperl/erl_embedded_redis.git", {tag, "3.0.3"}}}
    ]}.

    rebar get-deps compile

## Directly from terminal

    git clone git://github.com/gossiperl/erl_embedded_redis.git
    cd erl_embedded_redis
    ./rebar get-deps compile
    erl -pa ebin/ -pa deps/*/ebin
    {ok, R} = redis:start_link(server).
    {ok, Timestamp} = redis:lastsave(R).
    redis:stop(server, R).

# Supported operations

The `redis` module supports all Redis server commands except of `monitor`.

# Custom configuration

To start Redis with custom configuration, do the following:

    Cfg = [ { <<"port">>, <<"19000">> }, { <<"requirepass">>, <<"meh">> } ].
    {ok, R} = redis:start_link(server, Cfg).

All keys and values in the `Cfg` `proplist` must be binary.

If you already have a `redis.conf` file which you would like to use, do the following:

    CfgFromFile = redis_config:from_file( "/path/to/the/redis.conf" ).
    {ok, R} = redis:start_link(server, CfgFromFile).

# How does it work

`erl_embedded_redis` comes with a 64bit binaries of Redis for OS X and Linux. The binaries are in `priv` directory.

Issuing `redis:start_link(server)` opens an Erlang port, then an [eredis](https://github.com/wooga/eredis) client is created.

This client is used for all communication with the server and is also responsible for issuing `shutdown` command to Redis when `redis:stor(server, Pid)` is called.

The client will use a `hd(value)` of `<<"bind">>`, value of `<<"port">>` and value of `<<"requirepass">>` properties. The defaults (if not specified) are `"127.0.0.1"`, `6379` and `""` respecitely.

Every call to `redis:start_link(server)` will create a temp directory in the `priv` folder. A new `redis.conf` will be placed in there and the server will use that copy of the configuration. Issuing `redis:stop(server, ...)` will result in the temp folder being removed. Temporary configuration is a result of merge of the default `redis.conf` and given startup configuration where keys from given configuration take priority over defaults.

Windows is currently not supported and ARM not tested (most likely a separate redis build for ARM is required).

# Known issues

- `eredis` client is created at least 100ms after Redis is started; as `eredis` is required for stopping the server, the minimum time between `start_link` and `stop` should be at least 100ms
- if your Erlang process crashes, Redis will continue running, temporary configuration will not get cleaned up either

# License

Licensed under the Apache License, Version 2.0
