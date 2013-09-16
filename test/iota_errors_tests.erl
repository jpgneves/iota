-module(iota_errors_tests).

-include_lib("eunit/include/eunit.hrl").

emit_warning_test_() ->
  WarningResult = iota_result:add_warning(foo, {api, unrestricted_api}, iota_result:new()),
  [ ?_assertEqual(WarningResult, iota_errors:emit_warning(iota_result:new(), foo,
                                                          {api, unrestricted_api}))
  ].

emit_error_test_() ->
  ErrorResult = iota_result:add_error(foo, {api, {call_to_non_api_module, bar}}, iota_result:new()),
  [ ?_assertEqual(ErrorResult, iota_errors:emit_error(iota_result:new(), foo,
                                                     {api, {call_to_non_api_module, bar}}))
  ].
