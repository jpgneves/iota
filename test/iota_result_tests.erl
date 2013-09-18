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
  DefaultElement = [derp],
  ElementResult = iota_result:add_info(foo, bar, iota_result:new()),
  [ ?_assertEqual([], iota_result:lookup(foo, iota_result:new())),
    ?_assertEqual([bar], iota_result:lookup(foo, ElementResult)),
    ?_assertEqual([derp],
                  iota_result:lookup(herp, ElementResult, DefaultElement))
  ].
