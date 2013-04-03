-module(iota_utils).

-export([ get/2,
          get/3 ]).

get(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    false        -> throw(key_not_found);
    {Key, Value} -> Value
  end.

get(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.
