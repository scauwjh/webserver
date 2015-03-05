{
	application, logger,
	[
		{mod, {logger_app, []}},
		{description, ""},
		{vsn, "1.0.0"},
		{modules, [
			logger_app,
			logger_sup,
			logger
		]},
		{registered, [
			logger_sup,
			logger
		]},
		{applications, [
			kernel,
			stdlib
		]}
	]
}.