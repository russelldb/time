-module (test_all).

-include_lib("eunit/include/eunit.hrl").


all_test_() ->
	[{module, test_user_db},
	 {module, string_util_test}].
	
