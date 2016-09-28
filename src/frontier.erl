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

%% API functions
-export([push_url/1]).

%% Macros
-define(valid_file_type,"(.*?)\.(html)").
-define(file_type_in_path,"\\..*\\?").
-define(valid_url,"/((([A-Za-z]{3,9}:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)/").

%%%===================================================================
%%%%%% API functions
%%%%%%===================================================================

%% @doc interface to add a url to the frontier queue
-spec push_url(string()) -> ok | invalid_url | invalid_file_type | url_already_exists.
push_url(Url)->
	ok.
%% @doc checks if a file type is in url	
-spec has_file_type_in_path(string())-> has_file_type | no_file_type.
has_file_type_in_path(Url)->
	case re:run(Url,?file_type_in_path) of
		nomatch -> no_file_type;
		{match,_}-> has_file_type
	end.

%% @doc checks if url is pointing to an html file
-spec is_valid_file_type(string())-> ok | invalid_file_type. 
is_valid_file_type(Url)->
	case re:run(Url,?valid_file_type) of
		nomatch -> invalid_file_type;
		{match,_}-> ok
	end.

%% @doc checks if url has a valid format
-spec is_valid_url(string())-> ok | invalid_url.
is_valid_url(Url)->
	case re:run(Url,?valid_url) of
		nomatch -> invalid_url;
		{match,_}-> ok
	end.
		       

			       

