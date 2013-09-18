-module(iota_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  ExpectedHerp = {herp, [{is_api, true}, {api, all}]},
  ExpectedDerp = {derp, [{is_api, true}, {api, all}]},
  {setup,
   fun() ->
       compile:file("../priv/foo.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/xpto.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/herp.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/derp.erl", [{outdir, "../ebin"}])
   end,
   fun(_) ->
       ok = file:delete("../ebin/foo.beam"),
       ok = file:delete("../ebin/xpto.beam"),
       ok = file:delete("../ebin/herp.beam"),
       ok = file:delete("../ebin/derp.beam")
   end,
   [ ?_assertMatch([{_, [_|_]}|_],
                   wrap_xref(fun() -> iota_scanner:scan("..") end)),
     ?_assert(lists:member(ExpectedFoo,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedXpto,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedHerp,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedDerp,
                           wrap_xref(fun() -> iota_scanner:scan("..") end)))
   ]
  }.

scan_external_xref_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  ExpectedHerp = {herp, [{is_api, true}, {api, all}]},
  ExpectedDerp = {derp, [{is_api, true}, {api, all}]},
  {setup,
   fun() ->
       compile:file("../priv/foo.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/xpto.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/herp.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/derp.erl", [{outdir, "../ebin"}])
   end,
   fun(_) ->
       ok = file:delete("../ebin/foo.beam"),
       ok = file:delete("../ebin/xpto.beam"),
       ok = file:delete("../ebin/herp.beam"),
       ok = file:delete("../ebin/derp.beam")
   end,
   [ ?_assertMatch([{_, [_|_]}|_],
                   wrap_xref(fun() -> iota_scanner:scan("..") end)),
     ?_assert(lists:member(ExpectedFoo,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedXpto,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedHerp,
                           wrap_xref(fun() -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedDerp,
                           wrap_xref(fun() -> iota_scanner:scan("..") end)))
   ]
  }.

wrap_xref(Fun) ->
  iota_utils:with_xref(test_xref, Fun).
