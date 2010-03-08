-module(time_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_State) ->
    TimeChild = {time, {time, start_link, []}, permanent,
		 2000, worker, [time]},
    {ok, {{one_for_one, 10, 1}, [TimeChild]}}.
