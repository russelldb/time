%%%-------------------------------------------------------------------
%%% @author russellb <russellb@ubuntu.ubuntu-domain>
%%% @copyright (C) 2010, russellb
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2010 by russellb <russellb@ubuntu.ubuntu-domain>
%%%-------------------------------------------------------------------
-module(time).

-behaviour(gen_server).

%% API
-export([start_link/0, time/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("time.hrl").

-define(SERVER, ?MODULE). 
-define(BUCKET, <<"work">>).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, application:get_all_env(), []).

%%--------------------------------------------------------------------
%% @doc
%% Adds some time spent
%%
%% @spec add(time()) -> id()
%% @type time() :: #time{}
%% @end
%%--------------------------------------------------------------------
time(Time) when is_record(Time, time) ->
    gen_server:call(?SERVER, Time).





%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(RiakNode) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                          ignore |
%%                          {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    error_logger:info_msg("Env : ~p~n", [Args]),
    RiakNode = proplists:get_value(riak_node, Args),
    error_logger:info_msg("RiakNode : ~p~n", [RiakNode]),
    riak:client_connect(RiakNode).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Time, _From, State) when is_record(Time, time) ->
    Id = uuid:v4(),
    O = riak_object:new(?BUCKET, Id, Time),
    error_logger:info_msg("Putting : ~p~n", [Time]),
    Reply = {State:put(O, 1), Id},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

