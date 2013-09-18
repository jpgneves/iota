-module(iota_api_checks_tests).

-include_lib("eunit/include/eunit.hrl").

internal_consistency_test_() ->
  EmptyResults = iota_result:new(),
  ExpectedWarning = iota_errors:emit_warning(EmptyResults,
                                             foo, {api, unrestricted_api}),
  ExpectedError = iota_errors:emit_error(EmptyResults,
                                         foo,
                                         {api,
                                          {functions_in_api_but_not_exported,
                                           [{bar, 0}]
                                          }}),
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_api_checks),
       moka:replace(Moka, get_exports, fun(_) -> [{foo, 1}] end),
       moka:load(Moka),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch(ExpectedWarning,
                   iota_api_checks:internal_consistency({foo, [{is_api, true},
                                                               {api, all}]},
                                                        EmptyResults
                                                       )),
     ?_assertMatch(EmptyResults,
                   iota_api_checks:internal_consistency({foo, [{is_api, true},
                                                               {api, [{foo,1}]}
                                                              ]},
                                                        EmptyResults
                                                       )),
     ?_assertMatch(EmptyResults,
                   iota_api_checks:internal_consistency({foo, [{is_api, false},
                                                               {api, []}]},
                                                        EmptyResults
                                                       )),
     ?_assertMatch(EmptyResults,
                   iota_api_checks:internal_consistency({foo, [{is_api, false},
                                                               {api, [{foo,1}]
                                                               }]},
                                                        EmptyResults
                                                       )),
     ?_assertMatch(EmptyResults,
                   iota_api_checks:internal_consistency({foo, [{is_api, false},
                                                               {api, all}]},
                                                        EmptyResults
                                                       )),
     ?_assertMatch(ExpectedError,
                   iota_api_checks:internal_consistency({foo, [{is_api, true},
                                                               {api, [{bar,0}]}
                                                              ]},
                                                        EmptyResults
                                                       ))
     ]
  }.

external_calls_xref_error_test_() ->
  [ ?_assertThrow({error_running_xref, {unknown_constant, "foo"}},
                  iota_utils:with_xref(fun() ->
                                           iota_api_checks:external_calls(
                                             {foo, bar},
                                             iota_result:new()
                                            )
                                       end))
  ].
