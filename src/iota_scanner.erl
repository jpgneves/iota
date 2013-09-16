-module(iota_scanner).

-export([ scan/1 ]).

scan(Path) ->
  LibEbins = filelib:wildcard("ebin", filename:join(Path, "lib")),
  Beams = beams([filename:join(Path, "ebin"), LibEbins]),
  lists:map(fun(B) -> xref:add_module(iota_xref, B) end, Beams),
  [{list_to_atom(filename:rootname(filename:basename(B))),
    get_iota_data(B)} || B <- Beams].

beams(Paths) ->
  lists:foldl(fun(Path, Acc) ->
                  lists:append(Acc,
                               [filename:absname(F, Path)
                                || F <- filelib:wildcard("*.beam", Path)])
              end, [], Paths).

get_iota_data(Module) ->
  {ok, {_, [{attributes, Attrs}]}} = beam_lib:chunks(Module, [attributes]),
  IotaAttrs = iota_utils:get(iota, Attrs, []),
  Api0 = iota_utils:get(api, Attrs, []),
  IsApi = iota_utils:get(is_api, IotaAttrs, length(Api0) > 0),
  Api = case {IsApi, Api0} of
          {true, [all]} -> all;
          {true, []}    -> all;
          {true, [_|_]} -> Api0;
          {false, _}    -> []
        end,
  [{is_api, IsApi}, {api, Api}].
