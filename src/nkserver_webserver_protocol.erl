%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
%%
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
%%
%% -------------------------------------------------------------------

%% @doc
-module(nkserver_webserver_protocol).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([transports/1, default_port/1]).
-export([http_init/4]).

%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public
%% ===================================================================

%% Default options for resolve
transports(_) ->
    [http, https].


%% Default options for resolve
default_port(http) -> 80;
default_port(https) -> 443.


%% For HTTP-based protocol, function http_init is expected
%% See nkpacket_protocol

http_init([], _Req, _Env, NkPort) ->
    {ok, #{index_file:=Index}} = nkpacket:get_user_state(NkPort),
    {redirect, Index};

http_init([<<>>], Req, Env, NkPort) ->
    http_init([], Req, Env, NkPort);

http_init(_Paths, _Req, _Env, NkPort) ->
    {ok, _Class, {nkserver_webserver, _SrvId}} = nkpacket:get_id(NkPort),
    {ok, #{file_path:=Path}} = nkpacket:get_user_state(NkPort),
    {cowboy_static, {dir, Path}}.

