%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc Bit.ly API wrapper
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

-module(ebitly).
-author('Fernando Benavides <greenmellon@gmail.com>').

-behaviour(application).

%% API
-export([start/0, shorten/1]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% API (Global)
%% ===================================================================
%% @doc  Starts the application
-spec start() -> ok | {error, term()}.
start() -> application:start(ebitly).

%% ===================================================================
%% PUBLIC API
%% ===================================================================
%% @doc  For a long URL, <i>shorten/1</i> encodes a URL and returns a short one. 
-spec shorten(string()) -> {ok, string()} | {error, term()}.
shorten(LongUrl) -> ebitly_browser:shorten(LongUrl).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) -> ebitly_sup:start_link().

%% @hidden
-spec stop([]) -> ok.
stop([]) -> ok.