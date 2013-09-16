-module(iota_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
  Expected = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  {setup,
   fun() -> compile:file("../priv/foo.erl", [{outdir, "../ebin"}]) end,
   fun(_) -> ok = file:delete("../ebin/foo.beam") end,
   [ ?_assertMatch([{_, [_|_]}|_], iota_scanner:scan("..")),
     ?_assert(lists:member(Expected, iota_scanner:scan("..")))
   ]
  }.
