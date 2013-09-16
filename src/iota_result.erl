-module(iota_result).

-export([ add_error/3,
          add_warning/3,
          new/0,
          format/1,
          lookup/2
        ]).

new() -> orddict:new().

lookup(Module, Results) ->
  case orddict:find(Module, Results) of
    {ok, Value} -> Value;
    error       -> {{errors, []}, {warnings, []}}
  end.

format(Results) ->
  String = "===== IOTA results =====~n",
  F = fun({K, {{errors, E}, {warnings, W}}}, Acc) ->
          Acc ++ io_lib:format("~p - Errors: ~p, Warnings: ~p~n", [K, length(E), length(W)])
      end,
  L = orddict:to_list(Results),
  io:format(lists:foldl(F, String, L)).

add_warning(ModuleName, Warning, Results) ->
  Entry = lookup(ModuleName, Results),
  orddict:store(ModuleName, insert(warnings, Warning, Entry), Results).

add_error(ModuleName, Error, Results) ->
  Entry = lookup(ModuleName, Results),
  orddict:store(ModuleName, insert(errors, Error, Entry), Results).

insert(warnings, Warning, {E, {warnings, W}}) ->
  {E, {warnings, [Warning | W]}};
insert(errors, Error, {{errors, E}, W}) ->
  {{errors, [Error | E]}, W}.
