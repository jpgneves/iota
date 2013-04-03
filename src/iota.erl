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
  do_check([fun verify_api/2], scan(Path));
run_check(layer, Path) ->
  do_check([fun verify_layers/2], scan(Path));
run_check(all, Path) ->
  do_check([fun verify_api/2, fun verify_layers/2], scan(Path)).

do_check(Checkers, Info) ->
  R = lists:map(fun({M, _} = I) ->
                    lists:foldl(fun(C, Results) ->
                                    C(I, Results)
                                end, init_results(M), Checkers)
                end, Info),
  format_results(R).

verify_api(Data, Results) ->
  Checks = [ fun iota_api_checks:internal_consistency/2 ],
  lists:foldl(fun(C, R) ->
                  C(Data, R)
              end, Results, Checks).

verify_layers({_Module, _Info}, Results) ->
  Results.

init_results(ModuleName) ->
  {ModuleName, {errors, []}, {warnings, []}}.

format_results(Results) ->
  io:format("~p~n", [Results]).

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
  IotaAttrs = iota_utils:get(iota, Attrs, []),
  Layer = iota_utils:get(layer, IotaAttrs, undefined),
  Api = iota_utils:get(api, Attrs, []),
  IsApi = iota_utils:get(is_api, IotaAttrs, length(Api) > 0),
  [{layer, Layer}, {is_api, IsApi}, {api, Api}].
