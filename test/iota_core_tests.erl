-module(iota_core_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
  EmptyResult = iota_result:new(),
  [ ?_assertThrow(unrecognized_command, iota_core:run(foo, ".", [])),
    ?_assertMatch(EmptyResult, iota_core:run(check, ".",
                                             [{xref_server, iota_xref}])),
    ?_assertMatch(EmptyResult, iota_core:run(describe_api, ".",
                                             []))
  ].
