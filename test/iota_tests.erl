-module(iota_tests).

-include_lib("eunit/include/eunit.hrl").

main_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [ ?_assertMatch(ok, iota:main([])),
     ?_assertMatch(ok, iota:main(["api"])),
     ?_assertMatch(ok, iota:main(["all"])),
     ?_assertMatch(ok, iota:main(["api", "."])),
     ?_assertMatch(ok, iota:main(["all", "."])),
     ?_assertMatch(unrecognized_command, iota:main(["foo"])),
     ?_assertMatch(unrecognized_command, iota:main(["foo", "."]))
   ]
  }.
