%%%-------------------------------------------------------------------
%%% @author russellb <russellb@ubuntu.ubuntu-domain>
%%% @copyright (C) 2010, russellb
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2010 by russellb <russellb@ubuntu.ubuntu-domain>
%%% test change
%%%-------------------------------------------------------------------
-module(time).

-behaviour(gen_server).

%% API
-export([today_from_template/1, template/2,
	 start_link/0, add/1, list_templates/0, get_template/1, delete_template/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-include("time.hrl").

-define(SERVER, ?MODULE).

-define(WORK, <<"work">>).

-define(TEMPLATES, <<"templates">>).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  application:get_all_env(), []).

%%--------------------------------------------------------------------
%% @doc
%% Adds some time spent
%%
%% @spec add(time()) -> id()
%% @type time() :: #time{}
%% @end
%%--------------------------------------------------------------------
add(Time) when is_record(Time, time) ->
    gen_server:call(?SERVER, {add, Time}).

template(Name, TimeTemplate)
  when is_atom(Name), is_record(TimeTemplate, time) ->
    gen_server:call(?SERVER,
		    {register, Name, TimeTemplate}).


today_from_template(Name) when is_atom(Name) ->
    Template = get_template(Name),
    {Date, _} = erlang:localtime(),
    Time = Template#time{date = Date},
    gen_server:call(?SERVER, {add, Time}).

list_templates() ->
    gen_server:call(?SERVER, {list_templates}).

get_template(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {get_template, Name}).

delete_template(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {delete_template, Name}).

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
handle_call({add, Time}, _From, State)
  when is_record(Time, time) ->
    Id = uuid:v4(),
    O = riak_object:new(?WORK, Id, Time),
    error_logger:info_msg("Putting : ~p~n", [Time]),
    Reply = {State:put(O, 1), Id},
    {reply, Reply, State};
handle_call({register, Name, TimeTemplate}, _From, State)  when is_atom(Name), is_record(TimeTemplate, time) ->
    Bname = atom_to_binary(Name, utf8),
    case State:get(?TEMPLATES, Bname, 1) of
	{ok, O} ->
	    %%Update object
	    O2 = riak_object:update_value(O, TimeTemplate),
	    Reply = {State:put(O2, 1), Bname};
	{error, notfound} ->
	    O = riak_object:new(?TEMPLATES, Bname, TimeTemplate),
	    error_logger:info_msg("Creating template : ~p value: ~p ~n", [Bname, TimeTemplate]),
	    Reply = {State:put(O, 1), Bname};
	Ex ->
	    Reply = Ex
    end,
    {reply, Reply, State};
handle_call({get_template, Name}, _From, State)
  when is_atom(Name) ->
    case State:get(?TEMPLATES,
		   atom_to_binary(Name, utf8), 1) of
	{ok, O} ->
	    Reply = riak_object:get_value(O);
	{error, notfound} ->
	    Reply = undefined
    end,
    {reply, Reply, State};
handle_call({list_templates}, _From, State) ->
    {ok, Templates} = State:list_keys(?TEMPLATES),
    {reply, Templates, State};
handle_call({delete_template, Name}, _From, State) ->
    Reply = State:delete(?TEMPLATES, atom_to_binary(Name, utf8), 1),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

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
handle_cast(_Msg, State) -> {noreply, State}.

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
handle_info(_Info, State) -> {noreply, State}.

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
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


