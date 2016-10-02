
%% Copyright (c) 2016, Gregor Meyenberg  <gregor@meyenberg.de>
%% %%
%% %% Permission to use, copy, modify, and/or distribute this software for any
%% %% purpose with or without fee is hereby granted, provided that the above
%% %% copyright notice and this permission notice appear in all copies.
%% %%
%% %% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% %% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% %% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% %% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% %% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% %% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% %% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%

-module(downloader).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([download/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	url = "" :: string()
}).

-opaque state() :: #state{}.
-export_type([state/0]).

%% API.

-spec start_link(fun(()->string()|no_url_in_queue)) -> {ok, pid()}.
start_link(Pull_url) ->
	gen_server:start_link(?MODULE, [Pull_url], []).

%% @doc triggers a download of the current URL state
-spec download(pid())->ok.
download(Pid)->
	gen_server:cast(Pid,{download}).

%%===================================================================
%% gen_server callbacks
%%===================================================================

%% @private
%% @doc
%% Initializes the server
init([Pull_url]) ->
	case Pull_url() of
		no_url_in_queue -> {stop,normal}; 
		Url ->  {ok,#state{url=Url}}
	end.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({download},State)->
	{Header,Body}=downloader_http_client:get_request(State#state.url),
	ok=downloader_repository:add_file(State#state.url,Header,Body),
	{stop,normal,State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%===================================================================
%%%%% Internal functions
%%%%%===================================================================
%%

