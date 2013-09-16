-module(iota).

-export([ main/1 ]).

main([Check, ProjectPath | _]) ->
  try
    iota_core:check(list_to_atom(Check), ProjectPath)
  catch
    throw:unrecognized_command -> print_help(),
                                  unrecognized_command
  end;
main([Check | _]) ->
  {ok, Cwd} = file:get_cwd(),
  try
    iota_core:check(list_to_atom(Check), Cwd)
  catch
    throw:unrecognized_command -> print_help(),
                                  unrecognized_command
  end;
main([]) ->
  print_help().

print_help() ->
  io:format("usage: iota CHECK [PATH]~n~n  CHECK can be one of the following:~n"
            "    api   - check for api violations~n"
            "    all   - check for all types of violations~n").
