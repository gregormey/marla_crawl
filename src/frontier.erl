
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

-module(frontier).

-behaviour(gen_server).

%% API functions
-export([push_url/1]).
-export([start_link/0]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, code_change/3]).

%%%===================================================================
%%%%%% API functions
%%%%%%===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
	    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% @doc interface to add a url to the frontier queue. url will be rejected when:
%% - url is not valid
%% - is already in the queue
-spec push_url(string()) -> ok | invalid_url | invalid_file_type | url_exists.
push_url(Url)->
	gen_server:call(?MODULE,{push_url,Url}).

%% gen_server Function Definitions
%% @private
init(Args) ->
	frontier_repository:init(),
	{ok, Args}.
%% @private
handle_call({push_url,Url},_From,State)->
	{reply,
	 case frontier_url_validator:is_valid(Url) of
	 	ok -> case frontier_repository:is_url_existent(Url) of
			true->url_exists;
			false->frontier_repository:add_url(Url)
	 		end;
		ValidatorResult -> ValidatorResult
	end,
	State};
% @private
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%% @private
handle_cast(_Msg, State) ->
	{noreply, State}.
%% @private
handle_info(_Info, State) ->
	{noreply, State}.
%% @private
terminate(_Reason, _State) ->
	ok.
%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%===================================================================
%%%%%% Internals 
%%%%%%===================================================================




