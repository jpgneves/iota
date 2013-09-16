-module(iota_utils).

-export([ get/2,
          get/3,
          with_xref/1
        ]).

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

with_xref(Fun) ->
  xref:start(iota),
  try
    Fun()
  after
    xref:stop(iota)
  end.
