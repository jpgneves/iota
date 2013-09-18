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
          add_info/3,
          new/0,
          to_list/1,
          lookup/2,
          lookup/3
        ]).
-api([ lookup/2 ]).

-type iota_result() :: orddict:orddict().
-type result_info() :: {{errors, [term()]}, {warnings, [term()]}}.

%% @doc Create a new iota result aggregator
-spec new() -> iota_result().
new() -> orddict:new().

%% @doc Lookup for a key in the results
-spec lookup(Key::term(), Results::iota_result()) -> result_info() | term().
lookup(Key, Results) ->
  lookup(Key, Results, []).

-spec lookup(Key::term(), Results::iota_result(), Default::term())
            -> result_info() | term().
lookup(Key, Results, Default) ->
  case orddict:find(Key, Results) of
    {ok, Value} -> Value;
    error       -> Default
  end.

%% @doc Return the results as a list
-spec to_list(Results::iota_result()) -> [{atom(), term()}].
to_list(Results) ->
  orddict:to_list(Results).

%% @doc Add a Warning to the results for ModuleName.
-spec add_warning(ModuleName::module(), Warning::iota_errors:warning(),
                  Results::iota_result()) -> iota_result().
add_warning(ModuleName, Warning, Results) ->
  Entry = lookup(ModuleName, Results, {{errors, []}, {warnings, []}}),
  orddict:store(ModuleName, insert(warnings, Warning, Entry), Results).

%% @doc Add an Error to the results for ModuleName.
-spec add_error(ModuleName::module(), Warning::iota_errors:error(),
                Results::iota_result()) -> iota_result().
add_error(ModuleName, Error, Results) ->
  Entry = lookup(ModuleName, Results, {{errors, []}, {warnings, []}}),
  orddict:store(ModuleName, insert(errors, Error, Entry), Results).

%% @doc Add an entry to the results for Key.
-spec add_info(Key::term(), Info::term(), Results::iota_result())
              -> iota_result().
add_info(Key, Info, Results) ->
  Entry = lookup(Key, Results),
  orddict:store(Key, [Info | Entry], Results).

insert(warnings, Warning, {E, {warnings, W}}) ->
  {E, {warnings, [Warning | W]}};
insert(errors, Error, {{errors, E}, W}) ->
  {{errors, [Error | E]}, W}.
