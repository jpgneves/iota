-module(iota_api_checks_tests).

-include_lib("eunit/include/eunit.hrl").

internal_consistency_test_() ->
  EmptyResults = iota_result:new(),
  ExpectedWarning = iota_errors:emit_warning(EmptyResults,
                                             foo, {api, unrestricted_api}),
  [ ?_assertMatch(ExpectedWarning,
                  iota_api_checks:internal_consistency({foo, [{is_api, true},
                                                             {api, all}]},
                                                       EmptyResults
                                                      )),
    ?_assertMatch(EmptyResults,
                  iota_api_checks:internal_consistency({foo, [{is_api, true},
                                                              {api, [foo]}]},
                                                       EmptyResults
                                                      )),
    ?_assertMatch(EmptyResults,
                  iota_api_checks:internal_consistency({foo, [{is_api, false},
                                                              {api, []}]},
                                                       EmptyResults
                                                      ))
  ].

external_calls_xref_error_test_() ->
  [ ?_assertThrow({error_running_xref, {unknown_constant, "foo"}},
                  iota_utils:with_xref(fun() ->
                                           iota_api_checks:external_calls({foo, bar},
                                                                          iota_result:new())
                                       end))
  ].
