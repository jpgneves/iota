-module(iota_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  {setup,
   fun() ->
       compile:file("../priv/foo.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/xpto.erl", [{outdir, "../ebin"}])
   end,
   fun(_) ->
       ok = file:delete("../ebin/foo.beam"),
       ok = file:delete("../ebin/xpto.beam")
   end,
   [ ?_assertMatch([{_, [_|_]}|_], iota_scanner:scan("..")),
     ?_assert(lists:member(ExpectedFoo, iota_scanner:scan(".."))),
     ?_assert(lists:member(ExpectedXpto, iota_scanner:scan("..")))
   ]
  }.
