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
%%% @doc Utilities used in iota
%%% @author João Neves <sevenjp@gmail.com>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(iota_utils).

-export([ get/2,
          get/3,
          get_xref_server/0,
          start_xref_server/1,
          stop_xref_server/0,
          with_xref/2
        ]).

%% @doc Lookup Key in a Key-Value list and throw an exception if not found
-spec get(Key::term(), List::[{term(), term()}]) -> term() | no_result.
get(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    false        -> throw(key_not_found);
    {Key, Value} -> Value
  end.

%% @doc Lookup Key in a Key-Value list and return a default value if not found
-spec get(Key::term(), List::[{term(), term()}], Default::term()) -> term().
get(Key, List, Default) ->
  case lists:keyfind(Key, 1, List) of
    false        -> Default;
    {Key, Value} -> Value
  end.

%% @doc Run Fun with an xref server started and terminate the xref server
%% afterwards.
-spec with_xref(XrefServer::pid()|atom(), Fun::fun()) -> any().
with_xref(XrefServer, Fun) ->
  start_xref_server(XrefServer),
  try
    Fun()
  after
    stop_xref_server(),
    application:unset_env(iota, xref_server)
  end.

get_xref_server() ->
  {ok, XrefServer} = application:get_env(iota, xref_server),
  XrefServer.

start_xref_server(XrefServer) ->
  Server = maybe_start_xref_server(XrefServer),
  application:set_env(iota, xref_server, Server).

maybe_start_xref_server(XrefServer) when is_pid(XrefServer) ->
  XrefServer;
maybe_start_xref_server(XrefServer) ->
  case whereis(XrefServer) of
    undefined -> {ok, _} = xref:start(XrefServer);
    _         -> ok
  end,
  XrefServer.

stop_xref_server() ->
  case application:get_env(iota, xref_server) of
    undefined    -> ok;
    {ok, Server} -> xref:stop(Server)
  end,
  ok.
