-module(marla_crawl_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	ok=init_schema(),
	ensure_started(mnesia),
	ensure_started(inets),
	downloader_repository:init(),
	marla_crawl_sup:start_link().

stop(_State) ->
	ok.

%% @private
%% %% @doc
%% %% start all required applications
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

%% @doc
%% %% creates mnesia schema if not exists, if exists it ignores the error
%%
-spec init_schema() -> ok.
init_schema()->
	case mnesia:create_schema([node()]) of
		_ -> ok
	end.
