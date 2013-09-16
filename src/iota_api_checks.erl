-module(iota_api_checks).

-export([ internal_consistency/2,
          external_calls/2
        ]).

internal_consistency({Module, Info}, Results) ->
  case {iota_utils:get(is_api, Info), iota_utils:get(api, Info)} of
    {true, all}   -> iota_errors:emit_warning(Results,
                                              Module,
                                              {api, unrestricted_api});
    {true, [_|_]} -> Results;
    {false, _}    -> Results
  end.

external_calls({Module, _Info} = Data, Results) ->
  Query   = io_lib:format("XC || ~p", [Module]),
  {ok, R} = xref:q(iota_xref, Query),
  verify_external_calls(R, Data, Results).

verify_external_calls([{{CallerM, _, _}, {CalleeM, _, _}}|_], {Module, Info}, Results) ->
  case iota_utils:get(is_api, Info) of
    false -> iota_errors:emit_error(Results, CallerM, {api, {call_to_non_api_module, Module}})
  end.
