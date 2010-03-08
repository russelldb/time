compile: 
	mkdir -p ./test/ebin
	git submodule update --init
	@(cd ./deps/coverize; make)
	@(cd ./deps/erlang-uuid; make)
	erl -make
	erl -noshell -eval 'erl_tidy:dir("src",[verbose]).' -s erlang halt

clean:
	rm -rf ./ebin/*.beam
	rm -rf ./test/ebin/*.beam
	rm -rf ./coverage/*.html
	rm -f ./src/*.bak

coverage: compile
	mkdir -p coverage
	erl -noshell -pa $(RIAK_EBIN) -pa deps/coverize/ebin -pa deps/erlang-uuid/ebin/ -pa ebin -pa test/ebin -s eunit_helper run_cover -s init stop -name time_test -setcookie riak


rmbak:
	rm -f ./src/*.bak
