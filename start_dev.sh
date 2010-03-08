erl -pa ebin/ -pa deps/erlang-uuid/ebin/ -pa $RIAK_EBIN -n -name time -setcookie riak -boot start_sasl -eval "application:start(time)"
