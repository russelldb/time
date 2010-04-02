-module(test_time).

-include_lib("eunit/include/eunit.hrl").

-include("time.hrl").

%%The buckets
-define(WORK, <<"work">>).

-define(TEMPLATES, <<"templates">>).

setup() ->
    application:start(sasl),
    application:start(crypto),
    application:start(riak),
    application:start(luke),
    application:start(time),
    {ok, Riak} = riak:local_client(),
    Riak.


teardown(_) ->
    application:stop(time),
    application:stop(riak),
    application:stop(luke),
    application:stop(crytpo),
    application:stop(sasl).

write_template_test_ ( ) -> { setup , fun ( ) -> setup ( ) end , fun ( X ) -> teardown ( X ) end , fun generate_tests / 1 } .


generate_tests(Riak) ->
    generate_write_template(Riak) ++
	generate_update_template(Riak) ++
	generate_add(Riak) ++
	generate_today_from_template(Riak) ++
	generate_list_templates(Riak) ++
	generate_delete_template(Riak) ++
	generate_get_time(Riak) ++
	generate_delete_time(Riak) ++
	generate_get_all(Riak).

%% Get all mr
generate_get_all(Riak) ->
    empty_riak(Riak),
    {Date, _} = erlang:localtime(),
    Time = #time{client=list_to_binary("TestClient"), rate=55, rate_period=hour, units=7.5, date=Date},
    Time2 = #time{client=list_to_binary("TestClientToo"), rate=45, rate_period=hour, units=9, date=Date},
    Time3 = #time{client=list_to_binary("TestClientThree"), rate=500, rate_period=day, units=1, date=Date},
 
    {ok, UUID1} = time:add(Time),
    {ok, UUID2} = time:add(Time2),
    {ok, UUID3} = time:add(Time3),
 
    All = time:get_work(),
    [?_assert(lists:member(Time, All)), ?_assert(lists:member(Time2, All)), ?_assert(lists:member(Time3, All)), ?_assertEqual( 3, length(All))].

%% Delete time record
generate_delete_time(Riak) ->
    {Date, _} = erlang:localtime(),
    Time = #time{client=list_to_binary("TestClient"), rate=55, rate_period=hour, units=7.5, date=Date},
    {ok, UUID} = time:add(Time),
    {ok, Obj} = Riak:get(?WORK, UUID, 1),
    RetrievedTime = riak_object:get_value(Obj),
    Deleted = time:delete_time(binary_to_list(UUID)),
    {error, notfound} = Riak:get(?WORK, UUID, 1),
    ReDelete = time:delete_time(binary_to_list(UUID)),
    [?_assertMatch(Time, RetrievedTime),
     ?_assertMatch(ok, Deleted),
     ?_assertMatch({error, notfound}, ReDelete)].

%% Retrieve stored time record
generate_get_time(Riak) ->
    {Date, _} = erlang:localtime(),
    Time = #time{client=list_to_binary("TestClient"), rate=55, rate_period=hour, units=7.5, date=Date},
    {ok, Key} = time:add(Time),
    {ok, Obj} = Riak:get(?WORK, Key, 1),
    T = riak_object:get_value(Obj),
    FromTime = time:get_time(Key),
    FromTimeToo = time:get_time(binary_to_list(Key)),
    Unfound = time:get_time("Not even a UUID!"),
    [?_assertMatch(T, FromTime),
    ?_assertMatch(T, FromTimeToo),
    ?_assertMatch({error, notfound}, Unfound)].

%% Delete template
generate_delete_template(Riak) ->
    empty_riak(Riak),
    Template = #time{client = list_to_binary("TestClient"),
		     rate = 1, rate_period = day, units = 1},
    Res = time:template(test, Template),
    {ok, TKey} = Res,
    {ok, Obj} = Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),
    TResult = riak_object:get_value(Obj),
    ExpectedKey = atom_to_binary(test, utf8),
    ok = time:delete_template(test),
    DelRes= Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),
    [?_assertMatch(ExpectedKey, TKey),
     ?_assertMatch(Template, TResult),
     ?_assertMatch({error, notfound}, DelRes)].

%% Create a template
generate_write_template(Riak) ->
    Template = #time{client = list_to_binary("TestClient"),
		     rate = 1, rate_period = day, units = 1},
    Res = time:template(test, Template),
    {ok, TKey} = Res,
    {ok, Obj} = Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),
    TResult = riak_object:get_value(Obj),
    ExpectedKey = atom_to_binary(test, utf8),
    [?_assertMatch(ExpectedKey, TKey),
     ?_assertMatch(Template, TResult)].

%% If a template exists already add is update
generate_update_template(Riak) ->
    Template = #time{client = list_to_binary("TestClient"),
		     rate = 1, rate_period = day, units = 1},
    {ok, Key} = time:template(test, Template),
    {ok, OrigObj} =  Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),

    Changed = #time{client =list_to_binary("ChangedTestClient"),
		    rate = 7, rate_period = hour, units = 8},

    {ok, Key} = time:template(test, Changed),
    {ok, ChangedObj} = Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),

    OriginalResult = riak_object:get_value(OrigObj),
    ChangedResult = riak_object:get_value(ChangedObj),

    ExpectedKey = atom_to_binary(test, utf8),
    [?_assertMatch(ExpectedKey, Key),
     ?_assertMatch(Changed, ChangedResult),
     ?_assertMatch(Template, OriginalResult)].

%% Add a new time record
generate_add(Riak) ->
    {Date, _} = erlang:localtime(),
    Time = #time{client=list_to_binary("TestClient"), rate=55, rate_period=hour, units=7.5, date=Date},
    {ok, Key} = time:add(Time),
    {ok, Obj} = Riak:get(?WORK, Key, 1),
    T = riak_object:get_value(Obj),
    [?_assertMatch(Time, T)].

%% Today from template
generate_today_from_template(Riak) ->
    {Date, _} = erlang:localtime(),
    Template = #time{client = list_to_binary("TestClient"), rate = 1, rate_period = day, units = 1},
    {ok, _} = time:template(test, Template),
    {ok, Key} = time:today_from_template(test),
    Expected = Template#time{date=Date},
    {ok, O} = Riak:get(?WORK, Key, 1),
    Actual = riak_object:get_value(O),
    Wrong = time:today_from_template(wrong),
    [?_assertMatch(Expected, Actual), ?_assertMatch( {unknown_template,wrong}, Wrong)].

%%% Does list templates work?
generate_list_templates(Riak) ->
    empty_riak(Riak),
    Id1 = list_to_binary("TC1"),
    Template = #time{client = Id1,
		     rate = 1, rate_period = day, units = 1},
    O = riak_object:new(?TEMPLATES, Id1, Template),
    Riak:put(O, 1),
    Id2 = list_to_binary("TC2"),
    T2 = Template#time{client = Id2},
    O2 = riak_object:new(?TEMPLATES, Id2, T2),
    Riak:put(O2, 1),
    Templates = time:list_templates(),
    [?_assertMatch([Id1, Id2], Templates)].

%%% HELPERS
empty_riak(Riak) ->
    {ok, Buckets} = Riak:list_buckets(),
    empty_buckets(Riak, Buckets).

empty_buckets(_, []) ->
    ok;
empty_buckets(Riak, [H|T]) ->
    {ok, Keys} =  Riak:list_keys(H),
    empty_bucket(Riak, H, Keys),
    empty_buckets(Riak, T).

empty_bucket(_, _, []) ->
    ok;
empty_bucket(Riak, Bucket, [H|T]) ->
    Riak:delete(Bucket, H, 1),
    empty_bucket(Riak, Bucket, T).
