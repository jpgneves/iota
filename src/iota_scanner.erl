%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2013 João Neves
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Module scanner to extract information required by checks
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iota_scanner).

-export([ scan/1
        , scan/2
        ]).

-type directory()      :: string().
-type iota_info_item() :: {module(), term()}.
-type iota_info()      :: [iota_info_item()].

%% @doc Scan the given path and extract iota information from BEAM attributes
%% for all applications found in the path. Additionally, we add information
%% for all applications found in the iota xref server.
-spec scan(Path::directory()) -> [{module(), iota_info()}].
scan(Path) ->
  scan(Path, []).

%% @doc Scan the given path and extract iota information from BEAM attributes
%% for all applications found in the path. Additionally, we add information
%% for all applications found in the iota xref server.
-spec scan(Path::directory(), Options::iota_core:options())
          -> [{module(), iota_info()}].
scan(Path, Options) ->
  XrefServer = iota_utils:get_xref_server(),
  AbsPath = filename:absname(Path),
  Apps = get_applications(AbsPath, Options),
  lists:map(fun(A) ->
                case xref:add_application(XrefServer, A, [{warnings, false}]) of
                  {ok, _} = R -> R;
                  {error, _, {application_clash, _}} ->
                    N = generate_new_name(A),
                    xref:add_application(XrefServer, A, [{warnings, false},
                                                         {name, N}])
                end
            end, Apps),
  LibEbins = [filename:join([A, "ebin"]) || A <- Apps],
  code:add_paths(LibEbins),
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


generate_new_name(AppPath) ->
  [App, Prefix | _] = lists:reverse(filename:split(AppPath)),
  list_to_atom(string:join([Prefix, App], "_")).

get_applications(AbsPath, Options) ->
  LibDirs    = iota_utils:get(lib_dirs, Options,
                           [filename:join([AbsPath, "lib"])]),
  Ignores    = iota_utils:get(ignore_apps, Options, []),
  IgnoreDirs = [filename:join(L, atom_to_list(I))
                || I <- Ignores, L <- LibDirs],
  lists:foldl(fun(P, Acc) ->
                  [Acc | [filename:join([P, L])
                          || L <- filelib:wildcard("*", P)]]
              end, AbsPath, LibDirs) -- IgnoreDirs.
