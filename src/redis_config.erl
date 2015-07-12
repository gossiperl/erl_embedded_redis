-module(redis_config).

-export([ from_file/1,
          merge/2,
          make_temp_config/1,
          remove_temp_config/1,
          get_host/1,
          get_port/1,
          get_password/1 ]).

from_file(Path) ->
  case file:open(Path, [read]) of
    { ok, Device } ->
      { ok, process_file_lines(Device, []) };
    { error, Reason } ->
      { error, Reason }
  end.

process_file_lines(Device, Acc) ->
  case io:get_line(Device, "") of
    eof  -> file:close(Device), Acc;
    Line -> process_file_lines(Device, Acc ++ maybe_key_value( Line ))
  end.

maybe_key_value(Line) ->
  Stripped = string:strip(
               string:strip(
                 string:strip( Line, right, $\n ), right, $\r ) ),
  case list_to_binary( Stripped ) of
    <<"#", _/binary>> -> [];
    <<>> -> [];
    _ -> key_value( Stripped )
  end.

key_value(Line) ->
  K = string:substr(Line, 1, string:str(Line, " ")-1),
  V = string:substr(Line, string:str(Line, " ")+1, length(Line)-(length(K))),
  [ { list_to_binary(K), list_to_binary(V) } ].

merge(Config1, Config2) ->
  orddict:merge( fun(_K, _V1, V2) -> V2 end, Config1, Config2).

make_temp_config(Configuration) ->
  PrivDir = redis_server:privdir(),
  TempDir = PrivDir ++ "/Temp." ++ uuid:uuid_to_string(uuid:get_v4()),
  case file:make_dir(TempDir) of
    ok ->                write_configuration( TempDir, Configuration );
    { error, Reason } -> { error, Reason }
  end.

write_configuration(TempDir, Configuration) ->
  FileName = TempDir ++ "/redis.conf",
  { ok, Device } = file:open(FileName, [write]),
  lists:foreach(fun( { K, V } ) ->
    file:write( Device, <<K/binary, " ", V/binary, "\n">> )
  end, Configuration),
  _ = file:close(Device),
  { ok, FileName }.

remove_temp_config(FileName) ->
  _ = file:delete(FileName),
  file:del_dir(filename:dirname(FileName)).

get_host(Configuration) ->
  hd(string:tokens(binary_to_list(proplists:get_value(<<"bind">>, Configuration, <<"127.0.0.1">>)), " ")).

get_port(Configuration) ->
  list_to_integer(binary_to_list(proplists:get_value(<<"port">>, Configuration, <<"6379">>))).

get_password(Configuration) ->
  binary_to_list(proplists:get_value(<<"requirepass">>, Configuration, <<>>)).