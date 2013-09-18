-module(iota_utils_tests).

-include_lib("eunit/include/eunit.hrl").

get_test_() ->
  [ ?_assertEqual(bar, iota_utils:get(foo, [{foo, bar}])),
    ?_assertThrow(key_not_found, iota_utils:get(foo, [])),
    ?_assertEqual(bar, iota_utils:get(foo, [{foo, bar}], 42)),
    ?_assertEqual(42, iota_utils:get(foo, [], 42))
  ].

with_xref_test_() ->
  PersistXref = fun() ->
                    iota_utils:with_xref(test_xref, fun() -> ok end, true),
                    Result = whereis(test_xref) =/= undefined,
                    iota_utils:stop_xref_server(),
                    Result
                end,
  ExistingXref = fun() ->
                     {ok, TXref} = xref:start(foo_xref),
                     iota_utils:with_xref(TXref, fun() -> ok end)
                 end,
  [ ?_assertEqual(ok, iota_utils:with_xref(test_xref, fun() -> ok end)),
    ?_assertEqual(ok, ExistingXref()),
    ?_assert(PersistXref())
  ].

get_xref_server_test_() ->
  application:unset_env(iota, xref_server),
  [ ?_assertEqual(undefined, iota_utils:get_xref_server()) ].
