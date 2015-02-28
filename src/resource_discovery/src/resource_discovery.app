{
	application, resource_discovery,
	[
		{description, "A simple resource discovery system"},
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
		{applications, [kernel, stdlib]},
		{mod, {rd_app, []}}
	]
}.