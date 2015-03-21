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
        ]},
        {env,[
            {contact_nodes, ['contact1@172.16.28.132', 'contact2@172.16.28.132']},
            {wait_time, 2000}
        ]}
    ]
}.