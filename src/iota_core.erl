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
%%% @doc Entry point for running iota checks
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iota_core).

-export([ check/2 ]).

-type check_type() :: atom().
-type directory()  :: string().

%% @doc Run the specified checks for the given Path.
-spec check(Type::check_type(), Path::directory()) -> any().
check(api, Path) ->
  iota_utils:with_xref(fun() -> do_check([fun verify_api/2],
                                         iota_scanner:scan(Path)) end);
check(all, Path) ->
  iota_utils:with_xref(fun() -> do_check([fun verify_api/2],
                                         iota_scanner:scan(Path)) end);
check(_, _)      ->
  throw(unrecognized_command).

do_check(Checkers, Info) ->
  R = lists:foldl(fun(I, Acc) ->
                    lists:foldl(fun(C, Acc2) ->
                                    C(I, Acc2)
                                end, Acc, Checkers)
                end, iota_result:new(), Info),
  iota_result:format(R).

verify_api(Data, Results) ->
  Checks = [ fun iota_api_checks:internal_consistency/2,
             fun iota_api_checks:external_calls/2
           ],
  do_checks(Data, Checks, Results).

do_checks(Data, Checks, ResultsSoFar) ->
  lists:foldl(fun(C, R) ->
                  C(Data, R)
              end, ResultsSoFar, Checks).
