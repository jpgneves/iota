-module(iota_core).

-export([ check/2 ]).

check(api, Path) ->
  iota_utils:with_xref(fun() -> do_check([fun verify_api/2], iota_scanner:scan(Path)) end);
check(all, Path) ->
  iota_utils:with_xref(fun() -> do_check([fun verify_api/2], iota_scanner:scan(Path)) end);
check(_, _)      ->
  throw(unrecognized_command).

do_check(Checkers, Info) ->
  R = lists:foldl(fun(I, Acc) ->
                    lists:foldl(fun(C, Acc2) ->
                                    C(I, Acc2)
                                end, Acc, Checkers)
                end, iota_result:new(), Info),
  iota_result:format(R).

verify_api(Data, Results) ->
  Checks = [ fun iota_api_checks:internal_consistency/2,
             fun iota_api_checks:external_calls/2
           ],
  do_checks(Data, Checks, Results).

do_checks(Data, Checks, ResultsSoFar) ->
  lists:foldl(fun(C, R) ->
                  C(Data, R)
              end, ResultsSoFar, Checks).
