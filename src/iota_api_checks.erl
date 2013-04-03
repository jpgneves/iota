-module(iota_api_checks).

-export([ internal_consistency/2 ]).

internal_consistency({Module, Info}, Results) ->
  case {iota_utils:get(is_api, Info), iota_utils:get(api, Info)} of
    {true, []}    -> iota_errors:emit_warning(Results, Module, {api, unrestricted_api});
    {true, [_|_]} -> Results;
    {false, _}    -> Results
  end.
