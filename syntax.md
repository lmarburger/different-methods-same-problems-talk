> V = 1.
1

> V = 2.
** exception error: no match of right hand side value 2

> V = 1.
1


> List = [1, 2, 3, 4, 5].
[1,2,3,4,5]

> List2 = [0 | List].
[0,1,2,3,4,5]

> List3 = [-2, -1 | List2].
[-2,-1,0,1,2,3,4,5]


nth(N, List) -> Elem
    Types
        N = integer() >= 1
            1..length(List)
        List = [T, ...]
        Elem = T
        T = term()

Returns the Nth element of List.


> First = lists:nth(1, List3).
-2

> Second = lists:nth(2, List3).
-1

> Rest = lists:nthtail(2, List3).
[0,1,2,3,4,5]


> [V1, V2 | R] = List3.
[-2,-1,0,1,2,3,4,5]

> V1.
-2
> V2.
-1
> R.
[0,1,2,3,4,5]


> [First, Second | Rest] = List3.
[-2,-1,0,1,2,3,4,5]

> List4 = [-3 | List3].
[-3,-2,-1,0,1,2,3,4,5]

> [First, Second | Rest] = List4.
** exception error: no match of right hand side value [-3,-2,-1,0,1,2,3,4,5]



> Tuple = {a, b, c}.
{a,b,c}

> element(1, Tuple).
a


> {First, Rest}  = {a, b, c}.
** exception error: no match of right hand side value {a,b,c}


> {First, Second, Third}  = {a, b, c}.
{a,b,c}

> First.
a
> Second.
b
> Third.
c



{ok, Timer} = timer:send_after(1000, test).



case timer:send_after(1000, test) of
    {ok, Timer} ->
        Timer;
    {error, Message} ->
        % Some error handing here
end.



my_function(Arg1, Arg2) ->
    ok.


Response = timer:send_after(1000, test).
handle_timer(Response).

handle_timer({ok, Timer}) ->
    Timer;
handle_timer({error, Message}) ->
    % Some error handling here
    error.



backoff(0) -> 100;
backoff(1) -> 200;
backoff(2) -> 400;
backoff(3) -> 800;
backoff(4) -> 1600;
backoff(5) -> 3200;
backoff(_) -> 6400.


% In ruby:
% if attempts > 5
%   6400
% else
%   (2 ** attempts) * 100
% end
