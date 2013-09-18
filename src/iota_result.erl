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
  io:format("===== iota report =====~n"),
  F = fun({K, {{errors, E}, {warnings, W}}}, {AccE, AccW}) ->
          NewE = length(E),
          NewW = length(W),
          io:format("~p - Errors: ~p, Warnings: ~p~n", [K, NewE, NewW]),
          {AccE + NewE, AccW + NewW}
      end,
  L = orddict:to_list(Results),
  {TotalE, TotalW} = lists:foldl(F, {0, 0}, L),
  io:format("====================~nTotal - Errors:~p Warnings:~p~n",
            [TotalE, TotalW]).

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
