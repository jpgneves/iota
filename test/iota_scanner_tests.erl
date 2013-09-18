-module(iota_scanner_tests).

-include_lib("eunit/include/eunit.hrl").

scan_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  ExpectedHerp = {herp, [{is_api, true}, {api, all}]},
  ExpectedDerp = {derp, [{is_api, true}, {api, all}]},
  Wrap = fun(F) -> iota_utils:with_xref(F, []) end,
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
   [ ?_assertMatch([{_, [_|_]}|_], Wrap(fun(_) -> iota_scanner:scan("..") end)),
     ?_assert(lists:member(ExpectedFoo,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedXpto,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedHerp,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedDerp,
                           Wrap(fun(_) -> iota_scanner:scan("..") end)))
   ]
  }.

scan_external_xref_test_() ->
  ExpectedFoo  = {foo, [{is_api, true}, {api, [{foo, 1}]}]},
  ExpectedXpto = {xpto, [{is_api, false}, {api, []}]},
  ExpectedHerp = {herp, [{is_api, true}, {api, all}]},
  ExpectedDerp = {derp, [{is_api, true}, {api, all}]},
  XrefName     = test_xref,
  Wrap = fun(F) -> iota_utils:with_xref(F, [{xref_server, XrefName}]) end,
  {setup,
   fun() ->
       compile:file("../priv/foo.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/xpto.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/herp.erl", [{outdir, "../ebin"}]),
       compile:file("../priv/derp.erl", [{outdir, "../ebin"}]),
       {ok, _} = xref:start(XrefName)
   end,
   fun(_) ->
       ok = file:delete("../ebin/foo.beam"),
       ok = file:delete("../ebin/xpto.beam"),
       ok = file:delete("../ebin/herp.beam"),
       ok = file:delete("../ebin/derp.beam"),
       xref:stop(XrefName)
   end,
   [ ?_assertMatch([{_, [_|_]}|_], Wrap(fun(_) -> iota_scanner:scan("..") end)),
     ?_assert(lists:member(ExpectedFoo,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedXpto,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedHerp,
                           Wrap(fun(_) -> iota_scanner:scan("..") end))),
     ?_assert(lists:member(ExpectedDerp,
                           Wrap(fun(_) -> iota_scanner:scan("..") end)))
   ]
  }.
