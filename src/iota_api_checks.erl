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
%%% @doc Implementation of API violation checks
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(iota_api_checks).

-export([ internal_consistency/2,
          external_calls/2
        ]).

%% @doc Perform internal consistency checks on a module. Checks if the
%% declared API is a subset of the exported functions for a module
%% and emits a warning if Exported - API = {}, and an error if
%% API - Exported = {}.
-spec internal_consistency(Data::iota_scanner:iota_info_item(),
                           Results::iota_result:iota_result())
                          -> iota_result:iota_result().
internal_consistency(Data, Results) ->
  Checks = [ fun(R) -> unrestricted_api_check(Data, R) end,
             fun(R) -> unexported_api_check(Data, R) end
           ],
  lists:foldl(fun(F, Acc) -> F(Acc) end, Results, Checks).

%% @doc Check external calls between applications using xref.
%% Emits errors if an application calls another application's module which
%% is not declared as an API module, or if it calls a function that is not
%% declared as part of the API for that application through the module.
-spec external_calls(Data::iota_scanner:iota_info_item(),
                     Results::iota_result:iota_result())
                    -> iota_result:iota_result().
external_calls({Module, _Info} = Data, Results) ->
  Query   = lists:flatten(
              io_lib:format(
                "(Fun) ((App) (XC || ~p : Mod) - (App) (AE - strict AE))",
                [Module])),
  XrefServer = iota_utils:get_xref_server(),
  case xref:q(XrefServer, Query) of
    {ok, R} -> verify_external_calls(R, Data, Results);
    {error, xref_compiler, E} -> throw({error_running_xref, E})
  end.

unrestricted_api_check({Module, Info}, Results) ->
  case {iota_utils:get(is_api, Info), iota_utils:get(api, Info)} of
    {true, all}   -> iota_errors:emit_warning(Results,
                                              Module,
                                              {api, unrestricted_api});
    {true, [_|_]} -> Results;
    {false, _}    -> Results
  end.

unexported_api_check({Module, Info}, Results) ->
  Exports = get_exports(Module),
  Api = case iota_utils:get(api, Info) of
          all  -> Exports;
          List -> List
        end,
  ApiSet = sets:from_list(Api),
  ExportSet = sets:from_list(Exports),
  case sets:is_subset(ApiSet, ExportSet) of
    false -> UnexportedApi = sets:to_list(sets:subtract(ApiSet, ExportSet)),
             iota_errors:emit_error(Results, Module,
                                    {api, {functions_in_api_but_not_exported,
                                           UnexportedApi}});
    true  -> Results
  end.

get_exports(Module) ->
  Module:module_info(exports).



verify_external_calls([], _, Results) ->
  Results;
verify_external_calls([{Caller, {CalleeM, CalleeF, CalleeA} = Callee}| Rest],
                      {Module, Info}, Results) ->
  NewResults = case iota_utils:get(is_api, Info) of
                 false -> iota_errors:emit_error(Results, Caller,
                                                 {api,
                                                  {call_to_non_api_module,
                                                   CalleeM}});
                 true  -> Api = iota_utils:get(api, Info),
                          case lists:member({CalleeF, CalleeA}, Api) of
                            false ->
                              iota_errors:emit_error(Results, Caller,
                                                     {api,
                                                      {call_to_non_api_function,
                                                       Callee}});
                            true  -> Results
                          end
               end,
  verify_external_calls(Rest, {Module, Info}, NewResults).
