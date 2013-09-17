-module(iota_errors).

-export([ emit_warning/3,
          emit_error/3 ]).

emit_warning(Results, Source, Warning) ->
  io:format("WARNING: ~s~n", [format_warning(Source, Warning)]),
  iota_result:add_warning(Source, Warning, Results).

emit_error(Results, Source, Error) ->
  io:format("ERROR: ~s~n", [format_error(Source, Error)]),
  iota_result:add_error(Source, Error, Results).

format_warning(Source, {api, unrestricted_api}) ->
  lists:flatten(
    io_lib:format("Module ~p exports all functions as API functions.",
                  [Source])).

format_error({SourceM, SourceF, SourceA},
             {api, {call_to_non_api_module, TargetM}}) ->
  lists:flatten(
    io_lib:format("~p:~p/~p calls non-API module ~p~n",
                  [SourceM, SourceF, SourceA, TargetM]));
format_error({SourceM, SourceF, SourceA},
             {api, {call_to_non_api_function, {TModule, TFunction, TArity}}}) ->
  lists:flatten(
    io_lib:format("~p:~p/~p calls non-API function ~p:~p/~p~n",
                  [SourceM, SourceF, SourceA, TModule, TFunction, TArity]));
format_error(Module, {api, {functions_in_api_but_not_exported, Funs}}) ->
  lists:flatten(
    io_lib:format("Module ~p declares these functions as part of the API but "
                  "does not export them: ~p~n", [Module, Funs])).
