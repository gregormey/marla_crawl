{application, marla_crawl, [
	{description, "A web crawler to grap adresses "},
	{vsn, "0.0.1"},
	{modules, ['downloader','downloader_http_client','downloader_repository','frontier','frontier_repository','frontier_url_validator','marla_crawl_app','marla_crawl_sup','parser']},
	{registered, [marla_crawl_sup]},
	{applications, [kernel,stdlib,mnesia,inets,mnesia_utile]},
	{mod, {marla_crawl_app, []}}
]}.