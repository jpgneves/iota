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
%%% @doc Abstraction for iota check results
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(iota_result).

-export([ add_error/3,
          add_warning/3,
          new/0,
          format/1,
          lookup/2
        ]).

new() -> orddict:new().

lookup(Module, Results) ->
  case orddict:find(Module, Results) of
    {ok, Value} -> Value;
    error       -> {{errors, []}, {warnings, []}}
  end.

format(Results) ->
  io:format("===== iota report =====~n"),
  F = fun({K, {{errors, E}, {warnings, W}}}, {AccE, AccW}) ->
          NewE = length(E),
          NewW = length(W),
          io:format("~p - Errors: ~p, Warnings: ~p~n", [K, NewE, NewW]),
          {AccE + NewE, AccW + NewW}
      end,
  L = orddict:to_list(Results),
  {TotalE, TotalW} = lists:foldl(F, {0, 0}, L),
  io:format("====================~nTotal - Errors:~p Warnings:~p~n",
            [TotalE, TotalW]).

add_warning(ModuleName, Warning, Results) ->
  Entry = lookup(ModuleName, Results),
  orddict:store(ModuleName, insert(warnings, Warning, Entry), Results).

add_error(ModuleName, Error, Results) ->
  Entry = lookup(ModuleName, Results),
  orddict:store(ModuleName, insert(errors, Error, Entry), Results).

insert(warnings, Warning, {E, {warnings, W}}) ->
  {E, {warnings, [Warning | W]}};
insert(errors, Error, {{errors, E}, W}) ->
  {{errors, [Error | E]}, W}.
