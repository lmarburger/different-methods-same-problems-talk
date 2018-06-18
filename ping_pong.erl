-module(ping_pong).

-export([start/0, ping/2, pong/0]).

start() ->
    Pong_PID = spawn(ping_pong, pong, []),
    spawn(ping_pong, ping, [3, Pong_PID]).


pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.


ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("Ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n~n", [])
    end,
    ping(N - 1, Pong_PID).


% > c(ping_pong).
% {ok,ping_pong}

% > ping_pong:start().
% Pong received ping
% <0.129.0>
% Ping received pong

% Pong received ping
% Ping received pong

% Pong received ping
% Ping received pong

% Ping finished
% Pong finished
