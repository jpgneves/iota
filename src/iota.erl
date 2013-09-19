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
%%% @doc Escript entry point for iota
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(iota).

-export([ main/1 ]).

-spec main([string()]) -> ok | unrecognized_command.
main(["help" | _ ]) ->
  print_help();
main(["check" | _]) ->
  {ok, Cwd} = file:get_cwd(),
  handle_result("check", run("check", Cwd));
main(["describe-api" | _]) ->
  {ok, Cwd} = file:get_cwd(),
  handle_result("describe-api", run("describe-api", Cwd));
main([ProjectPath, Cmd | _]) ->
  handle_result(Cmd, run(Cmd, ProjectPath));
main([ProjectPath | _]) ->
  handle_result("check", run("check", ProjectPath));
main([]) ->
  {ok, Cwd} = file:get_cwd(),
  handle_result("check", run("check", Cwd)).

run(Command, Path) ->
  try
    iota_core:run(list_to_atom(Command), Path, [{xref_server, iota_xref}])
  catch
    throw:unrecognized_command -> print_help(),
                                  unrecognized_command
  end.

handle_result(_, unrecognized_command) -> halt(1);
handle_result("check", Result)         -> format_check(Result);
handle_result("describe-api", Result)  -> format_describe(Result).

print_help() ->
  io:format("usage: iota [PATH] [CMD]~n~n  CMD can be one of the following:~n"
            "    check        - check for all types of violations (default)~n"
            "    describe-api - describe the API of all applications").

format_describe(Result) ->
  print_report_header(),
  F = fun({App, Api}) ->
          io:format("API for ~p:~n  ~p~n", [App, lists:flatten(Api)])
      end,
  lists:map(F, Result).

format_check(Result) ->
  print_report_header(),
  F = fun({K, {{errors, E}, {warnings, W}}}, {AccE, AccW}) ->
          NewE = length(E),
          NewW = length(W),
          io:format("~p - Errors: ~p, Warnings: ~p~n", [K, NewE, NewW]),
          {AccE + NewE, AccW + NewW}
      end,
  {TotalE, TotalW} = lists:foldl(F, {0, 0}, Result),
  io:format("=======================~nTotal - Errors:~p Warnings:~p~n",
            [TotalE, TotalW]).

print_report_header() ->
  io:format("===== iota report =====~n").
