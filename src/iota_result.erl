-module(iota_result).

-export([ add_error/3,
          add_warning/3,
          new/0,
          format/1,
          lookup/2
        ]).

new() -> dict:new().

lookup(Module, Results) ->
  case dict:find(Module, Results) of
    {ok, Value} -> Value;
    error       -> {{errors, []}, {warnings, []}}
  end.

format(Results) ->
  io:format("~p~n", [Results]).

add_warning(ModuleName, Warning, Results) ->
  Entry = lookup(ModuleName, Results),
  dict:store(ModuleName, insert(warnings, Warning, Entry), Results).

add_error(ModuleName, Error, Results) ->
  Entry = lookup(ModuleName, Results),
  dict:store(ModuleName, insert(errors, Error, Entry), Results).

insert(warnings, Warning, {E, {warnings, W}}) ->
  {E, {warnings, [Warning | W]}};
insert(errors, Error, {{errors, E}, W}) ->
  {{errors, [Error | E]}, W}.
