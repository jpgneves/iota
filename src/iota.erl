-module(iota).

-export([ main/1 ]).

main([Check, ProjectPath | _]) ->
  iota_core:check(list_to_atom(Check), ProjectPath);
main([Check | _]) ->
  {ok, Cwd} = file:get_cwd(),
  iota_core:check(list_to_atom(Check), Cwd);
main([]) ->
  print_help().

print_help() ->
  io:format("usage: iota CHECK [PATH]~n~n  CHECK can be one of the following:~n"
            "    api   - check for api violations~n"
            "    all   - check for all types of violations~n").
