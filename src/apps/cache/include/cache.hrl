-ifndef(CACHE_H).
-define(CACHE_H, true).

%% default lease time
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

%% wait for resources init
-define(WAIT_FOR_RESOURCES, 2500).

%% wait for crating table
-define(WAIT_FOR_TABLES, 5000).

%% cache resources
-define(RESOURCES_NODES, resources_nodes).

-define(DEFAULT_WAITE_FOR_NODE, default_waite_for_node).

-define(DEFAULT_CONTACT_NODE, contact).

-endif.