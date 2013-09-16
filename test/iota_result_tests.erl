-module(iota_result_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  [ ?_assertEqual(orddict:new(), iota_result:new()) ].

add_error_test_() ->
  ErrorResult = orddict:store(foo, {{errors, [foo]}, {warnings, []}}, orddict:new()),
  [ ?_assertEqual(ErrorResult, iota_result:add_error(foo, foo, iota_result:new())) ].

add_warning_test_() ->
  WarningResult = orddict:store(foo, {{errors, []}, {warnings, [foo]}}, orddict:new()),
  [ ?_assertEqual(WarningResult, iota_result:add_warning(foo, foo, iota_result:new())) ].

lookup_test_() ->
  WElementResult = iota_result:add_warning(foo, bar, iota_result:new()),
  EElementResult = iota_result:add_error(foo, bar, iota_result:new()),
  [ ?_assertEqual({{errors, []}, {warnings, []}}, iota_result:lookup(foo, iota_result:new())),
    ?_assertEqual({{errors, [bar]}, {warnings, []}},
                  iota_result:lookup(foo, EElementResult)),
    ?_assertEqual({{errors, []}, {warnings, [bar]}},
                  iota_result:lookup(foo, WElementResult))
  ].
