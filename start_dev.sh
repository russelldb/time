erl -pa ebin/ -pa deps/erlang-uuid/ebin/ -pa $LUKE_EBIN -pa $RIAK_EBIN -n -name time -setcookie riak -boot start_sasl -eval "application:start(time)"
