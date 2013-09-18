-module(iota_utils_tests).

-include_lib("eunit/include/eunit.hrl").

get_test_() ->
  [ ?_assertEqual(bar, iota_utils:get(foo, [{foo, bar}])),
    ?_assertThrow(key_not_found, iota_utils:get(foo, [])),
    ?_assertEqual(bar, iota_utils:get(foo, [{foo, bar}], 42)),
    ?_assertEqual(42, iota_utils:get(foo, [], 42))
  ].

with_xref_test_() ->
  [ ?_assertEqual(ok, iota_utils:with_xref(test_xref, fun() -> ok end))
  ].
