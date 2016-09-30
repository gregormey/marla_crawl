
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
-module(frontier_repository).

%% API functions
-export([init/0]).
-export([is_url_existent/1]).
-export([add_url/1]).
-export([get_urls_for_download/0]).
-export([mark_url_as_pulled/1]).

-record(url, {
	      url = undefined :: string(),
	      pulled = false :: boolean(),
	      visited = false :: boolean()
	 }).

-opaque url() :: #url{}.
-export_type([url/0]).

%%%===================================================================
%%%%%% API functions
%%%%%%===================================================================

%% @doc init url table
%%
-spec init() -> ok | already_exists.
init() ->
	case mnesia:create_table(url, [{attributes, record_info(fields, url)},{disc_copies,[node()]}]) of
		{atomic,ok} -> ok;
		{aborted,{already_exists,stream}} -> already_exists
	end.

%% @doc checks if a url already exists in the repo
%%
-spec is_url_existent(string())-> true|false.
is_url_existent(Url)->
	case mnesia_utile:find_by_id(url,Url) of
		not_found -> false;
		_ -> true
	end.
%% @doc add a url to the queue
-spec add_url(string())-> ok | tuple().
add_url(Url)->	
	mnesia_utile:store(#url{url=Url}).

%% @doc finds all urls in the database that can be downloaded
-spec get_urls_for_download()-> list()| not_found.
get_urls_for_download()->
	case mnesia_utile:find(url, fun (R) -> 
					(false==R#url.pulled) and (false==R#url.visited) 
			       end)
	of
		not_found -> not_found;
		Urls ->[Url || #url{url=Url}<-Urls]
	end.

%% @doc marks a url as pulled to prevent that it can be pulled again
-spec mark_url_as_pulled(string())-> ok | not_found.
mark_url_as_pulled(Url)->
	case mnesia_utile:find_by_id(url,Url) of
		not_found -> not_found;
		UrlRecord -> mnesia_utile:store(UrlRecord#url{pulled=true})
	end.
