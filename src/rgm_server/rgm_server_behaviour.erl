-module(rgm_server_behaviour).
-author("kei 2015-02-15").
-description("web server behaviour").

-export([
    behaviour_info/1
]).

%% behaviour define
behaviour_info(callbacks) ->
    [
        {init, 1},
        {head, 3},
        {get, 3},
        {delete, 3},
        {options, 4},
        {post, 4},
        {put, 4},
        {trace, 4},
        {other_methods, 4}
    ];
behaviour_info(_other) ->
    undefined.