-module(test_app1_mod).

-api([ xpto/1 ]).
-export([ foo/0, xpto/1, xpto/2 ]).

foo() ->
  test_app2_mod:bar().

xpto(herp) ->
  xpto(herp, derp).

xpto(herp, derp) ->
  ok.
