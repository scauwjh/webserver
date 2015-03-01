{
	application, resource_discovery,
	[
		{mod, {rd_app, []}},
		{description, ""},
		{vsn, "1.0.0"},
		{modules, [
			resource_discovery,
			rd_app,
			rd_sup,
			rd_server
		]},
		{registered, [
			rd_sup,
			rd_server
		]},
		{applications, [
			kernel,
			stdlib
		]}
	]
}.