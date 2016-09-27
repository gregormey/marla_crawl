{application, marla_crawl, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['marla_crawl_app','marla_crawl_sup']},
	{registered, [marla_crawl_sup]},
	{applications, [kernel,stdlib]},
	{mod, {marla_crawl_app, []}}
]}.