compile: 
	mkdir -p ./test/ebin
	git submodule update --init
	@(cd ./deps/coverize; make)
	@(cd ./deps/erlang-uuid; make)
	erl -make

clean:
	rm -rf ./ebin/*.beam
	rm -rf ./test/ebin/*.beam
	rm -rf ./coverage/*.html
	rm -f ./src/*.bak

coverage: compile
	mkdir -p coverage
	erl -noshell -pa $(RIAK_EBIN) -pa $(RIAK)/apps/luke/ebin -pa deps/coverize/ebin -pa deps/erlang-uuid/ebin/ -pa ebin -pa test/ebin -s eunit_helper run_cover -s init stop -name time_test@127.0.0.1 -config test/app.config

rmbak:
	rm -f ./src/*.bak
	rm -f ./test/src/*.bak

tidy: compile
	erl -noshell -eval 'erl_tidy:dir("src",[verbose, {keep_unused, true}]).' -eval 'erl_tidy:dir("test/src",[verbose, {keep_unused, true}]).' -s erlang halt