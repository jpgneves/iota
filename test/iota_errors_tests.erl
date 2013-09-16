-module(iota_errors_tests).

-include_lib("eunit/include/eunit.hrl").

emit_warning_test_() ->
  [ ?_assertMatch({foo, {errors, []}, {warnings, [{api, unrestricted_api}]}},
                  iota_errors:emit_warning({foo, {errors, []}, {warnings, []}},
                                           foo, {api, unrestricted_api})),
    ?_assertMatch({foo, {errors, [bar]}, {warnings, []}},
                  iota_errors:emit_error({foo, {errors, []}, {warnings, []}},
                                         foo, bar))
  ].
