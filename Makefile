compile: 
	mkdir -p ./test/ebin
	git submodule update --init
	@(cd ./deps/coverize; make)
	erl -make
	
clean:
	rm -rf ./ebin/*.beam
	rm -rf ./test/ebin/*.beam
	rm -rf ./coverage/*.html
	
cover_test:
	erl -make -Dtest
	erl -noshell -pa ebin -pa test/ebin -s test_all test -s test_all cover_report -s init stop
	
coverage: compile
	mkdir -p coverage
	erl -noshell -pa deps/coverize/ebin -pa ebin -pa test/ebin -s eunit_helper run_cover -s init stop
