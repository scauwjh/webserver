-ifndef(RGM_SERVER_H).
-define(RGM_SERVER_H, true).

-define(DEFAULT_PORT, 1024).

-record(other_data, {
	last_modified,
	user_data
}).

-endif.