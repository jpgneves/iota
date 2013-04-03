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
  do_check([fun verify_api/1], scan(Path));
run_check(layer, Path) ->
  do_check([fun verify_layers/1], scan(Path));
run_check(all, Path) ->
  do_check([fun verify_api/1, fun verify_layers/1], scan(Path)).

do_check(Checkers, Info) ->
  lists:foreach(fun(I) ->
                    lists:foldl(fun(C, Results) ->
                                    [C(I) | Results]
                                end, [], Checkers)
                end, Info).

verify_api({Module, Info}) ->
  Result = case {get(is_api, Info), get(api, Info)} of
             {true, []}    -> emit_warning(Module, unrestricted_api);
             {true, [_|_]} -> ok;
             {false, _}    -> ok
           end,
  {Module, Result}.

verify_layers({_Module, _Info}) ->
  ok.

emit_warning(ModuleName, Warning) ->
  io:format("WARNING: ~s~n", [format_warning(ModuleName, Warning)]).

emit_error(ModuleName, Error) ->
  io:format("ERROR: ~s~n", [format_error(ModuleName, Error)]).

format_warning(ModuleName, unrestricted_api) ->
  lists:flatten(
    io_lib:format("Module ~p exports all functions as API functions.",
                  [ModuleName])).

format_error(_ModuleName, _Error) ->
  lists:flatten(
    io_lib:format("", [])).

scan(Path) ->
  LibEbins = filelib:wildcard("ebin", filename:join(Path, "lib")),
  Beams = beams([filename:join(Path, "ebin"), LibEbins]),
  [{list_to_atom(filename:rootname(filename:basename(B))),
    get_iota_data(B)} || B <- Beams].

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
  IsApi = get(is_api, IotaAttrs, length(Api) > 0),
  [{layer, Layer}, {is_api, IsApi}, {api, Api}].

get(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    false        -> throw(key_not_found);
    {Key, Value} -> Value
  end.

get(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.
