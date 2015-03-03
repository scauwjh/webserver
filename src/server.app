{
	application, server,
	[
		{mod, {server, []}},
		{description, ""},
		{vsn, "1.0.0"},
		{modules, [
			server
		]},
		{registered, []},
		{applications, [
			kernel,
			stdlib
		]},
		{env, [
			{port, 1024}
		]}
	]
}.