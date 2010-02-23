-module(eunit_helper).

-export([run_cover/0]).

run_cover() ->
  SourceDirs = [
				
				"./src/data/",
				"./src/util"
			
               ],
 
  coverize:run(SourceDirs, test_all).