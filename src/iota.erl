-module(iota).

-export([ main/1 ]).

main([Check, ProjectPath | _]) ->
  run_check(list_to_atom(Check), ProjectPath);
main([Check | _]) ->
  {ok, Cwd} = file:get_cwd(),
  run_check(list_to_atom(Check), Cwd);
main([]) ->
  print_help().

print_help() ->
  io:format("usage: iota CHECK [PATH]~n~n  CHECK can be one of the following:~n"
            "    api   - check for api violations~n"
            "    layer - check for layer violations~n"
            "    all   - check for all types of violations~n").

run_check(api, Path) ->
  scan(Path);
run_check(layer, Path) ->
  scan(Path);
run_check(all, Path) ->
  scan(Path).

scan(Path) ->
  LibEbins = filelib:wildcard("ebin", filename:join(Path, "lib")),
  Beams = beams([filename:join(Path, "ebin"), LibEbins]),
  [io:format("~p: ~p~n", [B, get_iota_data(B)]) || B <- Beams].

beams(Paths) ->
  lists:foldl(fun(Path, Acc) ->
                  lists:append(Acc,
                               [filename:absname(F, Path)
                                || F <- filelib:wildcard("*.beam", Path)])
              end, [], Paths).

get_iota_data(Module) ->
  {ok, {_, [{attributes, Attrs}]}} = beam_lib:chunks(Module, [attributes]),
  IotaAttrs = get(iota, Attrs, []),
  Layer = get(layer, IotaAttrs, undefined),
  Api = get(api, Attrs, []),
  IsApi = get(is_api, IotaAttrs, false) orelse length(Api) > 0,
  [{layer, Layer}, {is_api, IsApi}, {api, Api}].


get(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.
