-module(counter).

-behaviour(gen_server).

%% API
-export([start_link/0, count/0, increment/0, reset/0, crash/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {count = 0, started}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

count() ->
    gen_server:call(?MODULE, count, 5000).

increment() ->
    gen_server:cast(?MODULE, increment).

reset() ->
    gen_server:call(?MODULE, reset, 5000).

crash() ->
    gen_server:cast(?MODULE, {divide, 0}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Started = erlang:localtime(),
    {ok, #state{started = Started}}.

handle_call(count, _From, #state{count = Count} = State) ->
    {reply, Count, State};
handle_call(reset, _From, #state{count = Count, started = Started}) ->
    NewState = #state{started = erlang:localtime()},
    {reply, {Count, Started}, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(increment, #state{count = Count} = State) ->
    NewState = State#state{count = Count + 1},
    {noreply, NewState};
handle_cast({divide, Denominator}, _State) ->
    100 / Denominator;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
