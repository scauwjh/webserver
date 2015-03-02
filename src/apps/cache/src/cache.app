{
	application, cache,
	[
		{mod, {cache_app, []}},
		{description, ""},
		{vsn, "1.0.0"},
		{modules, [
			cache_app,
			cache_sup
		]},
		{registered, [
			cache_sup
		]},
		{applications, [
			kernel,
			stdlib
		]}
	]
}.