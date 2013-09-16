-module(test_app2_mod).

-export([ bar/0, baz/0 ]).

bar() ->
  42.

baz() ->
  test_app1_mod:xpto(herp, derp).
