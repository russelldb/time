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
    application:start(time),
    {ok, Riak} = riak:local_client(),
    Riak.


teardown(Riak) ->
    application:stop(time),
    {ok, WorkKeys} = Riak:list_keys(?WORK),
    {ok, TKeys} = Riak:list_keys(?TEMPLATES),
    delete_data(Riak, ?WORK, WorkKeys),
    delete_data(Riak, ?TEMPLATES, TKeys),
    application:stop(riak),
    application:stop(crytpo),
    application:stop(sasl).

delete_data(_, _, []) -> ok;
delete_data(Riak, Bucket, [Key | T]) ->
    Riak:delete(Bucket, Key, 1),
    delete_data(Riak, Bucket, T).

write_template_test_ ( ) -> { setup , fun ( ) -> setup ( ) end , fun ( X ) -> teardown ( X ) end , fun generate_tests / 1 } .


generate_tests(Riak) ->
    generate_write_template_tests(Riak) ++
	generate_update_template(Riak) ++
	generate_add(Riak) ++
	generate_today_from_template(Riak) ++
	generate_list_templates(Riak).

generate_write_template_tests(Riak) ->
    Template = #time{client = list_to_binary("TestClient"),
		     rate = 1, rate_period = day, units = 1},
    Res = time:template(test, Template),
    {ok, TKey} = Res,
    {ok, Obj} = Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),
    TResult = riak_object:get_value(Obj),
    ExpectedKey = atom_to_binary(test, utf8),
    [?_assertMatch(ExpectedKey, TKey),
     ?_assertMatch(Template, TResult)].

generate_update_template(Riak) ->
    Template = #time{client = list_to_binary("TestClient"),
		     rate = 1, rate_period = day, units = 1},
    {ok, TKey} = time:template(test, Template),
    Changed = #time{client =list_to_binary("ChangedTestClient"),
		    rate = 7, rate_period = hour, units = 8},
    {ok, TKey} = time:template(test, Changed),
    {ok, Obj} = Riak:get(?TEMPLATES, atom_to_binary(test, utf8), 1),
    TResult = riak_object:get_value(Obj),
    ExpectedKey = atom_to_binary(test, utf8),
    [?_assertMatch(ExpectedKey, TKey),
     ?_assertMatch(Changed, TResult)].


generate_add(Riak) ->
    {Date, _} = erlang:localtime(),
    Time = #time{client=list_to_binary("TestClient"), rate=55, rate_period=hour, units=7.5, date=Date},
    {ok, Key} = time:add(Time),
    {ok, Obj} = Riak:get(?WORK, Key, 1),
    T = riak_object:get_value(Obj),
    [?_assertMatch(Time, T)].

generate_today_from_template(Riak) ->
    {Date, _} = erlang:localtime(),
    Template = #time{client = list_to_binary("TestClient"), rate = 1, rate_period = day, units = 1},
    {ok, _} = time:template(test, Template),
    {ok, Key} = time:today_from_template(test),
    Expected = Template#time{date=Date},
    {ok, O} = Riak:get(?WORK, Key, 1),
    Actual = riak_object:get_value(O),
    [?_assertMatch(Expected, Actual)].

generate_list_templates(Riak) ->
    {ok, TKeys} = Riak:list_keys(?TEMPLATES),
    delete_data(Riak, ?TEMPLATES,  TKeys),
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
    Expected = [Id1, Id2],
    [?_assertMatch(Expected, Templates)].

