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
-module(nkserver_webserver_sample).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile(nowarn_export_all).
-compile(export_all).


-dialyzer({nowarn_function, start/0}).

-include_lib("nkserver/include/nkserver_module.hrl").


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Starts the service
start() ->
    {ok, _} = nkserver_webserver:start_link(test1, #{
        url => "https://node:9010/test1, http://node:9011/testB",
        debug => [http, nkpacket]
    }),
    {ok, _} = nkserver_webserver:start_link(test2, #{
        url => "http://node:9011/test2",
        file_path => "/tmp",
        debug => [http, nkpacket]
    }).


%% @doc Stops the service
stop() ->
    nkserver_srv_sup:stop(test1),
    nkserver_srv_sup:stop(test2).


test1() ->
    Url1 = "https://127.0.0.1:9010/test1/index.html",
    {ok, {{_, 200, _}, _, "<!DOC"++_}} = httpc:request(Url1),
    Url2 = "https://127.0.0.1:9010/test1/",
    {ok, {{_, 200, _}, _, "<!DOC"++_}} = httpc:request(Url2),
    Url3 = "https://127.0.0.1:9010/test1",
    {ok, {{_, 200, _}, _, "<!DOC"++_}} = httpc:request(Url3),
    Url4 = "https://127.0.0.1:9010/test1/dir/hi.txt",
    {ok, {{_, 200, _}, _, "nkserver"}} = httpc:request(Url4),
    Url5 = "http://127.0.0.1:9011/testB/dir/hi.txt",
    {ok, {{_, 200, _}, _, "nkserver"}} = httpc:request(Url5),
    Src = filename:join(code:priv_dir(nkserver_webserver), "www/dir/hi.txt"),
    {ok, _} = file:copy(Src, "/tmp/hi3.txt"),
    Url6 = "http://127.0.0.1:9011/test2/hi3.txt",
    {ok, {{_, 200, _}, _, "nkserver"}} = httpc:request(Url6),
    ok.


cow_test() ->
    Disp = cowboy_router:compile([{'_', [{"/test2/[...]", cowboy_static, {priv_dir, nkserver, "www"}}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 9012}], #{env => #{dispatch=>Disp}}).




