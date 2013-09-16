-module(iota_scanner).

-export([ scan/1 ]).

scan(Path) ->
  Apps = [Path | [filename:join([Path, "lib", L])
                  || L <- filelib:wildcard("*", filename:join(Path, "lib"))]],
  lists:map(fun(A) -> xref:add_application(iota_xref, A, [{warnings, false}]) end, Apps),
  LibEbins = filelib:wildcard("ebin", filename:join(Path, "lib")),
  Beams = beams([filename:join(Path, "ebin"), LibEbins]),
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
