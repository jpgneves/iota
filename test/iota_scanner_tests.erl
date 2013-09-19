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

scan_duplicate_application_name_test_() ->
  {setup,
   fun() ->
       Deps = sel_application:start_app(moka),
       Moka = moka:start(iota_scanner),
       moka:replace(Moka, xref, add_application, fun(_, "a/b", [{warnings, false}]) ->
                                                     {error, foo,
                                                      {application_clash, dummy}
                                                     };
                                                    (_, "a/b", [{warnings, false},
                                                              {name, a_b}]) ->
                                                     {ok, foo}
                                                 end),
       moka:replace(Moka, get_applications, fun(_,_) -> ["a/b"] end),
       moka:load(Moka),
       {Moka, Deps}
   end,
   fun({Moka, Deps}) ->
       moka:stop(Moka),
       sel_application:stop_apps(Deps)
   end,
   [ ?_assertMatch([], wrap_xref(fun() -> iota_scanner:scan("..") end))
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
