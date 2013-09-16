-module(iota_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  Wrap = fun(F) -> iota_utils:with_xref(F) end,
  {setup,
   fun() ->
       compile:file("../priv/foo.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/xpto.erl", [{outdir, "../ebin"}])
   end,
   fun(_) ->
       ok = file:delete("../ebin/foo.beam"),
       ok = file:delete("../ebin/xpto.beam")
   end,
   [ ?_assertMatch([{_, [_|_]}|_], Wrap(fun() -> iota_scanner:scan("..") end)),
     ?_assert(lists:member(ExpectedFoo, Wrap(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedXpto, Wrap(fun() -> iota_scanner:scan("..") end)))
   ]
  }.
