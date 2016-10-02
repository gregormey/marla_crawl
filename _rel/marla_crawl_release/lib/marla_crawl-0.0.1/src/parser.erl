-module(parser).
-compile([export_all]).

init()->
	inets:start().

download(Url)->
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	      httpc:request(Url),
	Body.

parse(links,Content)->
	{match,Captured}=re:run(Content,"(?<=href=(\"|')).+?(?=(\"|'))",[global]),
	[string:substr(Content,Start+1,Length) || [{Start, Length},_,_]<-Captured].

test()->
	parse(links,download("http://blog.gregormeyenberg.de/")).


