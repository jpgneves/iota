-module(iota_api_checks_tests).

-include_lib("eunit/include/eunit.hrl").

describe_api_test_() ->
  EmptyResults   = iota_result:new(),
  NotApiResults  = iota_result:add_info(bar, [], EmptyResults),
  SomeApiResults = iota_result:add_info(bar, [{foo,herp,0},{foo,derp,1}],
                                        EmptyResults),
  FullApiResults = iota_result:add_info(bar, [{foo,herp,0},
                                              {foo,derp,1},
                                              {foo,blah,2}],
                                        EmptyResults),
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_api_checks),
       moka:replace(Moka, xref, q, fun(test_xref, _) -> {ok, [bar]} end),
       moka:replace(Moka, get_exports, fun(_) ->
                                           [{herp,0},{derp,1},{blah,2}]
                                       end),
       moka:load(Moka),
       iota_utils:start_xref_server(test_xref),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       iota_utils:stop_xref_server(),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch(NotApiResults,
                   iota_api_checks:describe_api({foo, [{is_api, false},
                                                       {api, blah}]},
                                                EmptyResults
                                               )),
     ?_assertMatch(SomeApiResults,
                   iota_api_checks:describe_api({foo, [{is_api, true},
                                                       {api, [{herp,0},
                                                              {derp,1}]}]},
                                                EmptyResults
                                               )),
     ?_assertMatch(FullApiResults,
                   iota_api_checks:describe_api({foo, [{is_api, true},
                                                       {api, all}]},
                                                EmptyResults
                                               ))
   ]
  }.

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

external_calls_no_interapp_calls_test_() ->
  EmptyResults = iota_result:new(),
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_api_checks),
       moka:replace(Moka, xref, q, fun(test_xref, _) -> {ok, []} end),
       moka:load(Moka),
       iota_utils:start_xref_server(test_xref),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       iota_utils:stop_xref_server(),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch(EmptyResults,
                   iota_api_checks:external_calls({foo, [{is_api, true},
                                                         {api, all}]},
                                                  EmptyResults
                                                 ))
   ]
  }.

external_calls_non_api_module_test_() ->
  EmptyResults = iota_result:new(),
  ExpectedError = iota_errors:emit_error(EmptyResults,
                                         {bar, baz, 1},
                                         {api,
                                          {call_to_non_api_module, foo}
                                         }),
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_api_checks),
       moka:replace(Moka, xref, q, fun(test_xref, _) -> {ok, [{{bar, baz, 1},
                                                               {foo, bar, 1}
                                                              }
                                                             ]} end),
       moka:load(Moka),
       iota_utils:start_xref_server(test_xref),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       iota_utils:stop_xref_server(),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch(ExpectedError,
                   iota_api_checks:external_calls({foo, [{is_api, false},
                                                         {api, []}]},
                                                  EmptyResults
                                                 ))
   ]
  }.

external_calls_non_api_function_test_() ->
  EmptyResults = iota_result:new(),
  ExpectedError = iota_errors:emit_error(EmptyResults,
                                         {bar, baz, 1},
                                         {api,
                                          {call_to_non_api_function,
                                           {foo, bar, 1}}
                                         }),
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_api_checks),
       moka:replace(Moka, xref, q, fun(test_xref, _) -> {ok, [{{bar, baz, 1},
                                                               {foo, bar, 1}
                                                              }
                                                             ]} end),
       moka:load(Moka),
       iota_utils:start_xref_server(test_xref),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       iota_utils:stop_xref_server(),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch(ExpectedError,
                   iota_api_checks:external_calls({foo, [{is_api, true},
                                                         {api, [{herp,0}]}]},
                                                  EmptyResults
                                                 ))
   ]
  }.


external_calls_xref_error_test_() ->
  [ ?_assertThrow({error_running_xref, {unknown_constant, "foo"}},
                  iota_utils:with_xref(test_xref,
                                       fun() ->
                                           iota_api_checks:external_calls(
                                             {foo, bar},
                                             iota_result:new()
                                            )
                                       end))
  ].
