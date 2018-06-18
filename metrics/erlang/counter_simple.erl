-module(counter_simple).

-behaviour(gen_server).

%% API
-export([start_link/0, count/0, increment/0, crash/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

count() ->
    gen_server:call(?MODULE, count, 5000).

increment() ->
    gen_server:cast(?MODULE, increment).

crash() ->
    gen_server:cast(?MODULE, {divide, 0}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Count = 0,
    {ok, Count}.

handle_call(count, _From, Count) ->
    {reply, Count, Count};
handle_call(_Request, _From, Count) ->
    {reply, ok, Count}.

handle_cast(increment, Count) ->
    NewCount = Count + 1,
    {noreply, NewCount};
handle_cast({divide, Denominator}, _Count) ->
    100 / Denominator;
handle_cast(_Request, Count) ->
    {noreply, Count}.

handle_info(_Info, Count) ->
    {noreply, Count}.

terminate(_Reason, _Count) ->
    ok.

code_change(_OldVsn, Count, _Extra) ->
    {ok, Count}.

format_status(_Opt, Status) ->
    Status.
