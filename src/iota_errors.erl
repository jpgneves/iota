-module(iota_errors).

-export([ emit_warning/3,
          emit_error/3 ]).

add_warning({M, E, {warnings, W}}, Warning) ->
  {M, E, {warnings, [Warning | W]}}.

add_error({M, {errors, E}, W}, Error) ->
  {M, {errors, [Error | E]}, W}.

emit_warning(Results, ModuleName, Warning) ->
  io:format("WARNING: ~s~n", [format_warning(ModuleName, Warning)]),
  add_warning(Results, Warning).

emit_error(Results, ModuleName, Error) ->
  io:format("ERROR: ~s~n", [format_error(ModuleName, Error)]),
  add_error(Results, Error).

format_warning(ModuleName, {api, unrestricted_api}) ->
  lists:flatten(
    io_lib:format("Module ~p exports all functions as API functions.",
                  [ModuleName])).

format_error(ModuleName, {layer, invalid_layer_declaration}) ->
  lists:flatten(
    io_lib:format("Module ~p has an invalid layer declaration.", [ModuleName]));
format_error(ModuleName, {layer, too_many_layers}) ->
  lists:flatten(
    io_lib:format("Module ~p belongs to more than one layer.", [ModuleName])).
