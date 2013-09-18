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
%%% @doc Error generation and printing functions
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iota_errors).

-export([ emit_warning/3,
          emit_error/3 ]).

emit_warning(Results, Source, Warning) ->
  io:format("WARNING: ~s~n", [format_warning(Source, Warning)]),
  iota_result:add_warning(Source, Warning, Results).

emit_error(Results, Source, Error) ->
  io:format("ERROR: ~s~n", [format_error(Source, Error)]),
  iota_result:add_error(Source, Error, Results).

format_warning(Source, {api, unrestricted_api}) ->
  lists:flatten(
    io_lib:format("Module ~p exports all functions as API functions.",
                  [Source])).

format_error({SourceM, SourceF, SourceA},
             {api, {call_to_non_api_module, TargetM}}) ->
  lists:flatten(
    io_lib:format("~p:~p/~p calls non-API module ~p~n",
                  [SourceM, SourceF, SourceA, TargetM]));
format_error({SourceM, SourceF, SourceA},
             {api, {call_to_non_api_function, {TModule, TFunction, TArity}}}) ->
  lists:flatten(
    io_lib:format("~p:~p/~p calls non-API function ~p:~p/~p~n",
                  [SourceM, SourceF, SourceA, TModule, TFunction, TArity]));
format_error(Module, {api, {functions_in_api_but_not_exported, Funs}}) ->
  lists:flatten(
    io_lib:format("Module ~p declares these functions as part of the API but "
                  "does not export them: ~p~n", [Module, Funs])).
