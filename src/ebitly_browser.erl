%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc Queing browser for Bit.ly API
%%% @end
%%%-------------------------------------------------------------------

%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-module(ebitly_browser).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-define(TIMEOUT, 60*60*1000).
-define(BASE_URL, "http://api.bitly.com/v3/").
-define(QUERY_INTERVAL, 10). %% In ms. (100 queries per second)

-behaviour(gen_server).

-record(state, {queue = queue:new() :: queue(),
                base_params = ""    :: string()}).
-opaque state() :: #state{}.

-export([start_link/0, shorten/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec shorten(Url::string()) -> {ok, string()} | {error, term()}
%% @doc  For a long URL, <i>shorten/1</i> encodes a URL and returns a short one.
-spec shorten(string()) -> {ok, string()} | {error, term()}.
shorten(LongUrl) ->
  gen_server:call(?MODULE, {shorten, LongUrl}, infinity).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
  BaseParams =
    case {application:get_env(ebitly, login), application:get_env(ebitly, api_key)} of
      {undefined, undefined} -> "?no_params"; %%XXX: Just not to bother with &s later...
      {{ok, Login}, undefined} -> "?login=" ++ Login;
      {undefined, {ok, ApiKey}} -> "?apiKey=" ++ ApiKey;
      {{ok, Login}, {ok, ApiKey}} -> "?login=" ++ Login ++ "&apiKey=" ++ ApiKey
    end,
  {ok, #state{queue = queue:new(), base_params = BaseParams}}.

%% @hidden
-spec handle_call({shorten, string()}, reference(), state()) -> {noreply, state()}.
handle_call(Request, From, State = #state{queue = Q}) ->
  case queue:is_empty(Q) of
    true ->
      {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req);
    false ->
      ok
  end,
  {noreply, State#state{queue = queue:in({From, Request}, Q)}}.

%% @hidden
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

%% @hidden
-spec handle_info(handle_req, state()) -> {noreply, state(), hibernate}.
handle_info(handle_req, State = #state{queue = Q, base_params = BaseParams}) ->
  case queue:out(Q) of
    {empty, Q} ->
      {noreply, State, hibernate};
    {{value, {From, {shorten, LongUrl}}}, NewQ} ->
      Url = ?BASE_URL ++ "shorten/" ++ BaseParams ++ "&format=txt&longUrl=" ++
              ibrowse_lib:url_encode(LongUrl),
      try ibrowse:send_req(Url, [], get, [], [{connect_timeout, ?TIMEOUT}], ?TIMEOUT) of
        {ok, "200", _Headers, ShortUrl} ->
          {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
          gen_server:reply(From, {ok, re:replace(ShortUrl, "\n$", "", [{return, list}])}),
          {noreply, State#state{queue = NewQ}, hibernate};
        {ok, "500", _Headers, Error} ->
          {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
          gen_server:reply(From, {error, list_to_atom(Error)}),
          {noreply, State#state{queue = NewQ}, hibernate};
        {ok, "403", _Headers, Error} ->
          {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
          gen_server:reply(From, {error, list_to_atom(Error)}),
          {noreply, State#state{queue = NewQ}, hibernate};
        {ok, Code, _Headers, Error} ->
          {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
          gen_server:reply(From, {error, {http_error, Code, Error}}),
          {noreply, State#state{queue = NewQ}, hibernate};
        Error ->
          {ok, _TimerRef} = timer:send_after(?QUERY_INTERVAL, handle_req),
          gen_server:reply(From, {error, {internal_error, Error}}),
          {noreply, State#state{queue = NewQ}, hibernate}
      catch
        _:timeout -> %% An ibrowse internal process timed out
          gen_server:reply(From, {error, {internal_error, timeout}}),
          {noreply, State#state{queue = NewQ}, hibernate};
        _:{timeout, _} -> %% An ibrowse internal process timed out
          gen_server:reply(From, {error, {internal_error, timeout}}),
          {noreply, State#state{queue = NewQ}, hibernate};
        _:{conn_failed, {error, Error}} ->
          gen_server:reply(From, {error, {internal_error, Error}}),
          {noreply, State#state{queue = NewQ}, hibernate};
        _:Error ->
          gen_server:reply(From, {error, {internal_error, Error}}),
          {noreply, State#state{queue = NewQ}, hibernate}
      end
  end;
handle_info(_, State) -> {noreply, State, hibernate}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.