-module(iota_result).

-export([ new/1,
          format/1
        ]).

new(ModuleName) ->
  {ModuleName, {errors, []}, {warnings, []}}.

format(Result) ->
  io:format("~p~n", [Result]).
