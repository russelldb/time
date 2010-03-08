-module(eunit_helper).

-export([run_cover/0]).

run_cover() ->
    SourceDirs = ["./src/"],
    coverize:run(SourceDirs, test_all).
