-module(metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_link_shell/1]).

%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(ReportingInterval) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ReportingInterval]).

start_link_shell(ReportingInterval) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE,
                                      [ReportingInterval]),
    unlink(Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([ReportingInterval]) ->
    Counter = #{id => counter,
                start => {counter, start_link, []}},

    Reporter = #{id => reporter,
                 start => {reporter, start_link,
                           [counter, ReportingInterval]}},

    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    {ok, {SupFlags, [Counter, Reporter]}}.
