-module(iota_core_tests).

-include_lib("eunit/include/eunit.hrl").

check_test_() ->
  [ ?_assertThrow(unrecognized_command, iota_core:check(foo, ".")),
    ?_assertMatch(ok, iota_core:check(api, ".")),
    ?_assertMatch(ok, iota_core:check(all, "."))
  ].
