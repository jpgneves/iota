-module(foo).

-api([ foo/1 ]).

-export([ foo/1
        , bar/0
        ]).

foo(_) -> foo(41, 1).

foo(X, Y) -> X + Y.

bar() -> none.
