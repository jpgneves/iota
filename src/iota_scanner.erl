-module(iota_scanner).

-export([ scan/1 ]).

scan(Path) ->
  AbsPath = filename:absname(Path),
  Apps = [AbsPath | [filename:join([AbsPath, "lib", L])
                  || L <- filelib:wildcard("*", filename:join(AbsPath, "lib"))]],
  lists:map(fun(A) ->
                case xref:add_application(iota_xref, A, [{warnings, false}]) of
                  {ok, _} = R -> R;
                  {error, _, {application_clash, _}} ->
                    [App, Prefix | _] = lists:reverse(filename:split(A)),
                    N = list_to_atom(string:join([Prefix, App], "_")),
                    xref:add_application(iota_xref, A, [{warnings, false},
                                                        {name, N}])
                end
            end, Apps),
  LibEbins = [filename:join([A, "ebin"]) || A <- Apps],
  Beams = beams(LibEbins),
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
