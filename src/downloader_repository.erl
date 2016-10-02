
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
%%
-module(downloader_repository).

%% API functions
-export([init/0]).
-export([add_web_content/3]).
-export([get_web_content_body/1]).

-record(web_content, {
	      url = undefined :: string(),
	      header = undefined :: string(),
	      body = undefined :: string()
	 }).

-opaque web_content() :: #web_content{}.
-export_type([web_content/0]).

%%%===================================================================
%%%%%% API functions
%%%%%%===================================================================

%% @doc init web_content table
%%
-spec init() -> ok | already_exists.
init() ->
	case mnesia:create_table(web_content, [{attributes, record_info(fields, web_content)},{disc_copies,[node()]}]) of
		{atomic,ok} -> ok;
		{aborted,{already_exists,stream}} -> already_exists
	end.

%% @doc add a url to the queue
-spec add_web_content(string(),tuple(),string())-> ok | tuple().
add_web_content(Url,Header,Body)->	
	mnesia_utile:store(#web_content{url=Url,header=Header,body=Body}).

%% @doc retruns downloaded web content by a given url if content exists
-spec get_web_content_body(string())->string()|not_found.
get_web_content_body(Url)->
	 case mnesia_utile:find_by_id(web_content,Url) of
	 	not_found -> not_found;
		Web_content -> Web_content#web_content.body
	 end.
