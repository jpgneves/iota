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
main(Args) ->
  try docopt:docopt(doc(), string:join(Args, " ")) of
      Out -> start(Out)
  catch
    _:_ -> print_doc(),
           halt(1)
  end.

start(ParsedOpts) ->
  case is_help(ParsedOpts) of
    true  -> print_doc();
    false -> Path = get_path(ParsedOpts),
             Cmd  = get_command(ParsedOpts),
             handle_result(Cmd, run(Cmd, Path))
  end.

is_help(Opts) ->
  orddict:fetch("--help", Opts).

get_path(Opts) ->
  case orddict:fetch("<path>", Opts) of
    undefined -> {ok, Cwd} = file:get_cwd(),
                 Cwd;
    Val       -> Val
  end.

get_command(Opts) ->
  Commands = ["check", "describe_api"],
  Default  = "check",
  Pred     = fun(C) -> orddict:fetch(C, Opts) =:= true end,
  case lists:filter(Pred, Commands) of
    [Cmd] -> Cmd;
    []    -> Default
  end.

run(Command, Path) ->
  try
    Config  = read_config(Path),
    iota_core:run(list_to_atom(Command), Path, [{xref_server, iota_xref}
                                                | Config]
                 )
  catch
    throw:unrecognized_command -> print_doc(),
                                  unrecognized_command
  end.

handle_result(_, unrecognized_command) -> halt(1);
handle_result("check", Result)         -> format_check(Result),
                                          return_code(Result);
handle_result("describe_api", Result)  -> format_describe(Result),
                                          return_code(Result).

read_config(Path) ->
  case file:consult(filename:join(Path, "iota.config")) of
    {ok, Terms} -> Terms;
    {error, _}  -> []
  end.

return_code([]) ->
  halt();
return_code(_) ->
  halt(1).

print_doc() ->
  io:format("~s", [doc()]).

doc() ->
  lists:flatten(
    io_lib:format(
      "iota - A tool to help enforcing clean separation of responsabilities~n"
      "Usage:~n"
      "  iota (check | describe_api) [<path>]~n"
      "  iota -h | --help~n~n"
      "Options:~n"
      " -h --help         Show this screen~n",
     [])).

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
