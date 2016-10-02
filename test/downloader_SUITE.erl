
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
-module(downloader_SUITE).
-compile(export_all).

-import(ct_helper,[doc/1]).

all()->[download,fetch_content].

init_per_suite(_Config) ->
	{ok,Pid}=downloader:start_link(fun()-> "http://blog.gregormeyenberg.de" end),
	[Pid].

download([Pid])->
	doc("triggers a download"),
	ok=downloader:download(Pid).

fetch_content([_Pid])->
	doc("checks if downloaded content is available"),
	Body=downloader_repository:get_web_content_body("http://blog.gregormeyenber.de"),
	Body=/=not_found.	
