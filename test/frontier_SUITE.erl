
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

-module(frontier_SUITE).
-compile(export_all).

-import(ct_helper, [doc/1]).

all() -> 
	[push_url, pull_url].

init_per_suite(_Config) ->
	[].

push_url(_Config)->
	doc("Pushes diffrent URLs to frontier queue"),
	ok=frontier:push_url("http://blog.gregormeyenberg.de"),
	url_exists=frontier:push_url("http://blog.gregormeyenberg.de"),
	invalid_url=frontier:push_url("jjjjo/eee"),
	invalid_file_type=frontier:push_url("http://blog.gregormeyenberg.de/public/style.css?version=sss.html").

pull_url(_Config)->
	doc("Pulls URL twice from the queue"),
	Url=frontier:pull_url(),
	"http://blog.gregormeyenberg.de"=Url,
	no_url_in_queue=frontier:pull_url().


