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

-module(frontier_url_validator).

%% API functions
-export([is_valid/1]).

%% Macros
-define(valid_file_type,"(.*?)\.(html)").
-define(file_type_in_path,"\\..*\\?").
-define(valid_url,"/((([A-Za-z]{3,9}:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)/").

%%%===================================================================
%%%%%% API functions
%%%%%%===================================================================

%% @doc validates a url
-spec is_valid(string()) -> ok | invalid_url | invalid_file_type.
is_valid(Url)->
	validate(Url,[fun is_valid_url/1,fun is_valid_file_type/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
is_valid_test()->
	ok=is_valid("http://blog.gregormeyenberg.de"),
	invalid_url=is_valid("jjjjo/eee"),
	invalid_file_type=is_valid("http://blog.gregormeyenberg.de/public/style.css?version=sss.html"),
	ok.
-endif.

%%%%===================================================================
%%%%%% Internal Validators 
%%%%%%===================================================================

%% @doc checks if url has a valid format
-spec is_valid_url(string())-> ok | invalid_url.
is_valid_url(Url)->
	case re:run(Url,?valid_url) of
		nomatch -> invalid_url;
		{match,_}-> ok
	end.

%% @doc checks if url is pointing to an html file
-spec is_valid_file_type(string())-> ok | invalid_file_type. 
is_valid_file_type(Url)->
	case has_file_type_in_path(Url) of
		has_file_type ->
				case re:run(Url,?valid_file_type) of
					nomatch -> invalid_file_type;
					{match,_}-> ok
				end;
		no_file_type -> ok
	end.			
%%%%===================================================================
%%%%%% Internals 
%%%%%%===================================================================

% @doc checks if a file type is in url	
-spec has_file_type_in_path(string())-> has_file_type | no_file_type.
has_file_type_in_path(Url)->
	case re:run(Url,?file_type_in_path) of
		nomatch -> no_file_type;
		{match,_}-> has_file_type
	end.       

%% @doc perfroms a list of validators for a url and stops is one result is not ok 
-spec validate(string(), list()) -> ok | invalid_url | invalid_file_type.
validate(Url,[Validator|Validators])->
	case Validator(Url) of
		ok -> validate(Url,Validators);
		Result->Result
	end;
validate(_Url,[])->
	ok.      

