-module(reporter).

-behaviour(gen_server).

%% API
-export([start_link/2, crash/0, interval/0, report_now/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(state, {instrument, interval, timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Instrument, Interval) ->
    Args = [Instrument, Interval],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

interval() ->
    gen_server:call(?MODULE, interval).

crash() ->
    gen_server:cast(?MODULE, {divide, 0}).

report_now() ->
    gen_server:call(?MODULE, report_now).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Instrument, Interval]) ->
    {ok, Timer} = timer:send_after(Interval, report),
    {ok, #state{instrument = Instrument,
                interval = Interval,
                timer = Timer}}.

handle_call(interval, _From, #state{interval = Interval} = State) ->
    {reply, Interval, State};
handle_call(report_now, _From, #state{timer = Timer} = State) ->
    {ok, cancel} = timer:cancel(Timer),
    self() ! report,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({divide, Denominator}, _State) ->
    100 / Denominator;
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(report, #state{instrument = Instrument,
                           interval = Interval} = State) ->
    Data = Instrument:reset(),
    io:format("Report: ~p~n", [Data]),

    {ok, Timer} = timer:send_after(Interval, report),
    {noreply, State#state{timer = Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
