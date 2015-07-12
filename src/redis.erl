-module(redis).

-export([start_link/1, start_link/2, stop/2, stop/3, get_output/1]).
-export([ bgwriteaof/1,
          bgsave/1, 
          client_kill/2,
          client_list/1,
          client_getname/1,
          client_pause/2,
          client_setname/2,
          command/1,
          command_count/1,
          command_getkeys/1,
          command_info/2,
          config_get/2,
          config_rewrite/1,
          config_set/3,
          config_resetstat/1,
          dbsize/1,
          debug_object/2,
          debug_segfault/1,
          flushdb/1,
          flushall/1,
          info/1,
          info/2,
          lastsave/1,
          monitor/1,
          role/1,
          save/1,
          slaveof/3, 
          slowlog/3,
          slowlog/2, 
          time/1 ]).

% @doc Start Redis server without any configuration, uses Redis defaults.
start_link(server) ->
  redis_server:start_link().

% @doc Start Redis server with configuration. Configuration will be merged with default configuration.
start_link(server, Configuration) when is_list(Configuration) ->
  redis_server:start_link(Configuration).

% @doc Stop Redis without saving.
stop(server, Pid) ->
  stop(server, Pid, nosave).
stop(server, Pid, nosave) ->
  gen_server:cast( Pid, {stop, nosave} );

% @doc Stop Redis with save.
stop(server, Pid, save) ->
  gen_server:cast( Pid, {stop, save} ).

% @doc Get produced server output.
get_output(Pid) ->
  gen_server:call( Pid, get_output ).

%% ------------------------
%% Redis Server operations:
%% ------------------------

% @doc Asynchronously rewrite the append-only file.
bgwriteaof(Pid) ->
  gen_server:call(Pid, { command, ["BGWRITEAOF"] }).

% @doc Asynchronously save the dataset to disk.
bgsave(Pid) ->
  gen_server:call(Pid, { command, ["BGSAVE"] }).

% @doc Kill the connection of a client.
client_kill(Pid, Client) when is_binary(Client) ->
  gen_server:call(Pid, { command, ["CLIENT", "KILL", Client] }).

% @doc Get the list of client connections.
client_list(Pid) ->
  gen_server:call(Pid, { command, ["CLIENT", "LIST"] }).

% @doc Get the current connection name.
client_getname(Pid) ->
  gen_server:call(Pid, { command, ["CLIENT", "GETNAME"] }).

% @doc Stop processing commands from clients for some time.
client_pause(Pid, Timeout) when is_integer(Timeout) ->
  gen_server:call(Pid, { command, ["CLIENT", "PAUSE", Timeout] }).

% @doc Set the current connection name.
client_setname(Pid, Name) when is_binary(Name) ->
  gen_server:call(Pid, { command, ["CLIENT", "SETNAME", Name] }).

% @doc Get array of Redis command details.
command(Pid) ->
  gen_server:call(Pid, { command, ["COMMAND"] }).

% @doc Get total number of Redis commands.
command_count(Pid) ->
  gen_server:call(Pid, { command, ["COMMAND", "COUNT"] }).

% @doc Extract keys given a full Redis command.
command_getkeys(Pid) ->
  gen_server:call(Pid, { command, ["COMMAND", "GETKEYS"] }).

% @doc Get array of specific Redis command details.
command_info(Pid, Command) when is_binary(Command) ->
  gen_server:call(Pid, { command, ["COMMAND", "INFO", Command] }).

% @doc Get the value of a configuration parameter.
config_get(Pid, Parameter) when is_binary(Parameter) ->
  gen_server:call(Pid, { command, ["CONFIG", "GET", Parameter] }).

% @doc Rewrite the configuration file with the in memory configuration.
config_rewrite(Pid) ->
  gen_server:call(Pid, { command, ["CONFIG", "REWRITE"] }).

% @doc Set a configuration parameter to the given value.
config_set(Pid, Parameter, Value) when is_binary(Parameter) andalso ( is_binary(Value) orelse is_number(Value) orelse is_boolean(Value) ) ->
  gen_server:call(Pid, { command, ["CONFIG", "SET", Parameter, Value] }).

% @doc Reset the stats returned by INFO.
config_resetstat(Pid) ->
  gen_server:call(Pid, { command, ["CONFIG", "RESETSTAT"] }).

% @doc Return the number of keys in the selected database.
dbsize(Pid) ->
  gen_server:call(Pid, { command, ["DBSIZE"] }).

% @doc Get debugging information about a key.
debug_object(Pid, Key) when is_binary(Key) ->
  gen_server:call(Pid, { command, ["DEBUG", "OBJECT", Key] }).

% @doc Make the server crash.
debug_segfault(Pid) ->
  gen_server:call(Pid, { command, ["DEBUG", "SEGFAULT"] }).

% @doc Remove all keys from all databases.
flushall(Pid) ->
  gen_server:call(Pid, { command, ["FLUSHALL"] }).

% @doc Remove all keys from the current database.
flushdb(Pid) ->
  gen_server:call(Pid, { command, ["FLUSHDB"] }).

% @doc Get information and statistics about the server.
info(Pid) ->
  gen_server:call(Pid, { command, ["INFO"] }).

% @doc Get information and statistics about the server.
info(Pid, Section) when is_binary(Section) ->
  gen_server:call(Pid, { command, ["INFO", Section] }).

% @doc Get the UNIX time stamp of the last successful save to disk.
lastsave(Pid) ->
  gen_server:call(Pid, { command, ["LASTSAVE"] }).

% @doc Listen for all requests received by the server in real time (not implemented).
monitor(_Pid) ->
  not_implemented.

% @doc Return the role of the instance in the context of replication.
role(Pid) ->
  gen_server:call(Pid, { command, ["ROLE"] }).

% @doc Synchronously save the dataset to disk.
save(Pid) ->
  gen_server:call(Pid, { command, ["SAVE"] }).

% @doc Make the server a slave of another instance, or promote it as master.
slaveof(Pid, Host, Port) when is_binary(Host) andalso is_integer(Port) ->
  gen_server:call(Pid, { command, ["SLAVEOF", Host, Port] }).

% @doc Manages the Redis slow queries log - get first Count elements of the log.
slowlog(Pid, get, Count) when is_integer(Count) ->
  gen_server:call(Pid, { command, ["SLOWLOG", "GET", Count] }).

% @doc Manages the Redis slow queries log - get log.
slowlog(Pid, get) ->
  gen_server:call(Pid, { command, ["SLOWLOG", "GET"] });

% @doc Manages the Redis slow queries log - get the length of the log.
slowlog(Pid, len) ->
  gen_server:call(Pid, { command, ["SLOWLOG", "LEN"] });

% @doc Manages the Redis slow queries log - reset the log.
slowlog(Pid, reset) ->
  gen_server:call(Pid, { command, ["SLOWLOG", "RESET"] }).

% @doc Return the current server time.
time(Pid) ->
  gen_server:call(Pid, { command, ["TIME"] }).
