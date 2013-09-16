-module(iota).

-export([ main/1 ]).

-spec main([string()]) -> ok | unrecognized_command.
main([Check, ProjectPath | _]) ->
  handle_result(run(Check, ProjectPath));
main([Check | _]) ->
  {ok, Cwd} = file:get_cwd(),
  handle_result(run(Check, Cwd));
main([]) ->
  print_help().

run(Check, Path) ->
  try
    iota_core:check(list_to_atom(Check), Path)
  catch
    throw:unrecognized_command -> print_help(),
                                  unrecognized_command
  end.

handle_result(ok) -> ok;
handle_result(unrecognized_command) -> halt(1).

print_help() ->
  io:format("usage: iota CHECK [PATH]~n~n  CHECK can be one of the following:~n"
            "    api   - check for api violations~n"
            "    all   - check for all types of violations~n").
