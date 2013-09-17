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
  Query   = lists:flatten(
              io_lib:format(
                "(Fun) ((App) (XC || ~p : Mod) - (App) (AE - strict AE))",
                [Module])),
  case xref:q(iota_xref, Query) of
    {ok, R} -> verify_external_calls(R, Data, Results);
    {error, xref_compiler, E} -> throw({error_running_xref, E})
  end.

verify_external_calls([], _, Results) ->
  Results;
verify_external_calls([{Caller, {CalleeM, CalleeF, CalleeA} = Callee}| Rest],
                      {Module, Info}, Results) ->
  NewResults = case iota_utils:get(is_api, Info) of
                 false -> iota_errors:emit_error(Results, Caller,
                                                 {api, {call_to_non_api_module, CalleeM}});
                 true  -> Api = iota_utils:get(api, Info),
                          case lists:member({CalleeF, CalleeA}, Api) of
                            false -> iota_errors:emit_error(Results, Caller,
                                                            {api, {call_to_non_api_function,
                                                             Callee}});
                            true  -> Results
                          end
               end,
  verify_external_calls(Rest, {Module, Info}, NewResults).
