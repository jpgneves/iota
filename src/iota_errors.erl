-module(iota_errors).

-export([ emit_warning/3,
          emit_error/3 ]).

emit_warning(Results, ModuleName, Warning) ->
  io:format("WARNING: ~s~n", [format_warning(ModuleName, Warning)]),
  iota_result:add_warning(ModuleName, Warning, Results).

emit_error(Results, ModuleName, Error) ->
  io:format("ERROR: ~s~n", [format_error(ModuleName, Error)]),
  iota_result:add_error(ModuleName, Error, Results).

format_warning(ModuleName, {api, unrestricted_api}) ->
  lists:flatten(
    io_lib:format("Module ~p exports all functions as API functions.",
                  [ModuleName])).

format_error(ModuleName, {api, {call_to_non_api_module, TargetModule}}) ->
  lists:flatten(
    io_lib:format("Non-API module ~p called by ~p~n", [TargetModule, ModuleName]));
format_error(ModuleName, {api, {call_to_non_api_function, {TModule, TFunction, TArity}}}) ->
  lists:flatten(
    io_lib:format("Non-API function ~p:~p/~p called by ~p~n",
                  [TModule, TFunction, TArity, ModuleName])).
