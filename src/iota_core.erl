-module(iota_core).

-export([ check/2 ]).

check(api, Path) ->
  do_check([fun verify_api/2], iota_scanner:scan(Path));
check(all, Path) ->
  do_check([fun verify_api/2], iota_scanner:scan(Path)).

do_check(Checkers, Info) ->
  R = lists:map(fun({M, _} = I) ->
                    lists:foldl(fun(C, Results) ->
                                    C(I, Results)
                                end, iota_result:new(M), Checkers)
                end, Info),
  iota_result:format(R).

verify_api(Data, Results) ->
  Checks = [ fun iota_api_checks:internal_consistency/2 ],
  do_checks(Data, Checks, Results).

do_checks(Data, Checks, ResultsSoFar) ->
  lists:foldl(fun(C, R) ->
                  C(Data, R)
              end, ResultsSoFar, Checks).
