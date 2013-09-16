-module(iota_api_checks_tests).

-include_lib("eunit/include/eunit.hrl").

internal_consistency_test_() ->
  EmptyResults = iota_result:new(foo),
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
