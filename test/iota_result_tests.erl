-module(iota_result_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  [ ?_assertEqual({foo, {errors, []}, {warnings, []}}, iota_result:new(foo)) ].
