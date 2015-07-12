-module(redis_server).

-behaviour(gen_server).

-export([start_link/0, start_link/1, init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2, code_change/3]).
-export([privdir/0]).

-record(state, { redis_conn :: port(),
                 redis_conf :: list(),
                 redis_conf_path :: list(),
                 eredis_pid :: any(),
                 redis_output :: list() }).

-define(PING_INTERVAL, 30000).

start_link() ->
  { ok, DefaultConfig } = redis_config:from_file( privdir() ++ "/redis.conf" ),
  gen_server:start_link(?MODULE, [ DefaultConfig ], []).

start_link(Configuration) ->
  { ok, DefaultConfig } = redis_config:from_file( privdir() ++ "/redis.conf" ),
  MergedConfig = redis_config:merge( DefaultConfig, Configuration ),
  gen_server:start_link(?MODULE, [ MergedConfig ], []).

init([ Configuration ]) ->
  Host = redis_config:get_host(Configuration),
  Port = redis_config:get_port(Configuration),
  Auth = redis_config:get_password(Configuration),
  { ok, ConfigurationPath } = redis_config:make_temp_config( Configuration ),
  % get executable path:
  RedisPath = get_redis_path(),
  error_logger:info_msg("Starting Redis from ~p with configuration ~p.~n", [ RedisPath, ConfigurationPath ]),
  % Start Redis:
  P = open_port({spawn_executable, RedisPath}, [stderr_to_stdout, in, exit_status, stream, {line, 1024}, { args, [ ConfigurationPath ] }]),
  true = port_connect( P, self() ),
  % Start eredis client for the server:
  erlang:send_after(100, self(), { create_client, self(), Host, Port, Auth }),
  % Start ping timer:
  erlang:send_after(?PING_INTERVAL, self(), ping),
  {ok, #state{ redis_conn = P,
               redis_conf = Configuration,
               redis_conf_path = ConfigurationPath,
               eredis_pid = undefined,
               redis_output = [] }}.

% @doc Stop server with nosave option.
handle_cast({stop, nosave}, State = #state{ eredis_pid = Eredis }) ->
  _ = stop( Eredis, "NOSAVE" ),
  { noreply, State };

% @doc Stop server with save option.
handle_cast({stop, save}, State = #state{ eredis_pid = Eredis }) ->
  _ = stop( Eredis, "SAVE" ),
  { noreply, State }.

handle_info({create_client, From, Host, Port, Auth}, State) ->
  case (From == self()) of
    true ->
      case eredis:start_link(Host, Port, 0, Auth) of
        {ok, Eredis} ->
          error_logger:info_msg("Eredis client connected to ~p:~p.~n", [Host, Port]),
          { noreply, State#state{ eredis_pid = Eredis } };
        {error, Reason} ->
          error_logger:info_msg("Could not create Redis client because of an error: ~p. Attempting again shortly...~n", [ Reason ]),
          erlang:send_after(100, self(), { create_client, self(), Host, Port, Auth }),
          { noreply, State }
      end;
    false ->
      { noreply, State }
  end;

% @doc Ping server and schedule next ping execution.
handle_info(ping, State = #state{ eredis_pid = Eredis }) ->
  _ = eredis:q(Eredis, ["PING"]),
  erlang:send_after(?PING_INTERVAL, self(), ping),
  { noreply, State };

% @doc Handle redis process termination.
handle_info({Port, {exit_status, Status}}, State = #state{ redis_conn = Port, eredis_pid = Eredis, redis_conf_path = ConfigurationPath }) ->
  case Status of
    0 ->
      error_logger:error_msg("Redis stopping on request.~n");
    _ ->
      error_logger:error_msg("Redis disappeared unexpectedly, about to exit with code ~p!~n", [ Status ])
  end,
  _ = stop_eredis(Eredis),
  Port ! {self(), close},
  _ = redis_config:remove_temp_config( ConfigurationPath ),
  erlang:send_after(1000, self(), { redis_gone, { exit_status, Status } }),
  { noreply, State };

handle_info({ redis_gone, { exit_status, Status } }, State) ->
  case Status of
    0 ->
      { stop, normal, State };
    _ ->
      exit({ redis_gone, { exit_status, Status } })
  end;

% @doc Redis produced output, read it and store.
handle_info({_P, { data, { eol, Data } } }, State = #state{ redis_output = RedisOutput }) ->
  case RedisOutput of
    undefined ->
      {noreply, State};
    _ ->
      {noreply, State#state{ redis_output = RedisOutput ++ [ Data ] } }
  end.

% @doc Return all output produced by Redis so far.
handle_call(get_output, _From, State = #state{ redis_output = RedisOutput }) ->
  {reply, RedisOutput, State};

handle_call({command, Command}, _From, State = #state{ eredis_pid = Eredis }) ->
  { reply, eredis:q(Eredis, Command), State }.

terminate(_Reason, _State) ->
  {ok, noreply}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% @doc Stop the server gracefully.
stop(Eredis, MaybeSave) ->
  eredis:q(Eredis, ["SHUTDOWN", MaybeSave]).

% @doc Stop eredis connection.
stop_eredis(Eredis) ->
  error_logger:info_msg("Stopping eredis."),
  case Eredis of
    undefined -> ok;
    _ -> eredis:stop(Eredis)
  end.

% @doc Get Redis executable path.
get_redis_path() ->
  PrivDir = privdir(),
  case os:type() of
    {unix, darwin} ->
      PrivDir ++ "/redis-server.app";
    {unix, _} ->
      PrivDir ++ "/redis-server";
    {win32, _} ->
      %PrivDir ++ "/redis-server.exe";
      PrivDir ++ "/unsupported-os";
    _ ->
      PrivDir ++ "/unsupported-os"
  end.

% @doc Find out the priv directory for this program.
privdir() ->
  case code:priv_dir(erl_embedded_redis) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      % we might be running outside of application?
      MaybeEbin = string:sub_string(EbinDir, length(EbinDir)-3, length(EbinDir)),
      case MaybeEbin of
        "ebin" ->
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
        _ ->
          filename:join(EbinDir, "priv")
      end;
    Dir -> Dir
  end.