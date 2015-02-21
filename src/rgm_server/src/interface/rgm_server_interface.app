{
	application, rgm_server_interface,
	[
		{mod, {rgm_server_interface_app, []}},
		{description, ""},
		{vsn, "1.0.0"},
		{modules, [
			rgm_server_interface_app,
			rgm_server_interface_sup,
			rgm_server_interface
		]},
		{registered, [
			rgm_server_interface_sup,
			rgm_server_interface
		]},
		{applications, [
			kernel,
			stdlib
		]},
		{env, [
			{port, 1024}
		]}
	]
}.