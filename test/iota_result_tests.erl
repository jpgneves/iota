-module(iota_result_tests).

-include_lib("eunit/include/eunit.hrl").

new_test_() ->
  [ ?_assertEqual(dict:new(), iota_result:new()) ].

add_error_test_() ->
  ErrorResult = dict:store(foo, {{errors, [foo]}, {warnings, []}}, dict:new()),
  [ ?_assertEqual(ErrorResult, iota_result:add_error(foo, foo, iota_result:new())) ].

add_warning_test_() ->
  WarningResult = dict:store(foo, {{errors, []}, {warnings, [foo]}}, dict:new()),
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
