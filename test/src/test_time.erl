-module(test_time).

-include_lib("eunit/include/eunit.hrl").

-include("time.hrl").

%%The buckets
-define(WORK, <<"work">>).
-define(TEMPLATES, <<"templates">>).


setup() ->
    application:start(time),
    {ok, Riak} = riak:client_connect('riak@127.0.0.1'),
    Riak.

teardown(Riak) ->
    application:stop(time),
    {ok, WorkKeys} = Riak:list_keys(?WORK),
    {ok, TKeys} =  Riak:list_keys(?TEMPLATES),
    delete_data(Riak, ?WORK, WorkKeys),
    delete_data(Riak, ?TEMPLATES, TKeys).

delete_data(_, _, []) ->
    ok;
delete_data(Riak, Bucket, [Key|T]) ->
    Riak:delete(Bucket, Key, 1),
    delete_data(Riak, Bucket, T).


write_template_test_() ->
    {setup, fun() -> setup() end, 
     fun(X) -> teardown(X) end,
     fun generate_write_template_tests/1}.


generate_write_template_tests(Riak) ->
    Template = #time{client=list_to_binary("TestClient"), rate=1, rate_period=day, units=1},
    {ok, TKey} = time:create_template(test, Template),
    {ok, Obj} = Riak:get(?TEMPLATES,  atom_to_binary(test, utf8), 1),
    TResult = riak_object:get_value(Obj),
    
    [?_assertMatch(test, TKey),
    ?_assertMatch(Template, TResult)].
    
    
