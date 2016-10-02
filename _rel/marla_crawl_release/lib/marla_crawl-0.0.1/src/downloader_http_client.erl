
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
-module(downloader_http_client).

%% API
-export([get_request/1]).

%% @doc makes a get request and returns content + headers of a given URL 
-spec get_request(string())-> {ok,tuple(),string()}|{error,tuple()}.
get_request(Url)->
	erlang:display(Url),
	erlang:display(httpc:request(Url)),
	erlang:display("check"),
	case httpc:request(Url) of
		{ok, {{_Version, 200, _ReasonPhrase}, Headers, Body}}->{ok,Headers,Body};
		BadRequest->{error,BadRequest}
	end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
get_test()->
	{ok,_,_}=get("http://www.google.de"),
	{error,_,_}=get("dwdwdwewd"),
	ok.
-endif.

