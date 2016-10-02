
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

all()->[download].

init_per_suite(_Config) ->
	[].

download(_Config)->
	doc("triggers a download and checks if conten is available after 5 secounds"),
	{ok,Pid}=downloader:start_link(fun()-> "http://blog.gregormeyenberg.de" end),
	ok=downloader:download(Pid),
	timer:sleep(timer:seconds(5)),
	Body=downloader_repository:get_web_content_body("http://blog.gregormeyenberg.de"),
	true=(not_found=/=Body).

get_pid_from_config(Config)->
	{pid,Pid}=lists:last(Config),
	Pid.
