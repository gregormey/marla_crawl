{application, marla_crawl, [
	{description, "A web crawler to grap adresses "},
	{vsn, "0.0.1"},
	{modules, ['frontier','frontier_repository','frontier_url_validator','marla_crawl_app','marla_crawl_sup','parser']},
	{registered, [marla_crawl_sup]},
	{applications, [kernel,stdlib,mnesia,mnesia_utile]},
	{mod, {marla_crawl_app, []}}
]}.