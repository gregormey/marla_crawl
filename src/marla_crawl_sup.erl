-module(marla_crawl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Frontier = ?CHILD(frontier, worker),
	Children = [Frontier],
	RestartStrategy = {one_for_one, 10, 60},
	{ok, {RestartStrategy, Children}}.
