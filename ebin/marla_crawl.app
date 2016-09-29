{application, marla_crawl, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['frontier_url_validator','marla_crawl_app','marla_crawl_sup','parser']},
	{registered, [marla_crawl_sup]},
	{applications, [kernel,stdlib]},
	{mod, {marla_crawl_app, []}}
]}.