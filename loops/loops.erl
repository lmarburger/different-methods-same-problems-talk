#!/usr/bin/env escript

main(_) ->
    List = [82, 71, 12, 60, 49, 72, 36, 35, 6, 97],
    io:format("List: ~p~n", [List]),

    MaxFold = list_max_fold(List),
    io:format("Fold: ~p~n", [MaxFold]),

    MaxRecurse = list_max_recurse(List),
    io:format("Recurse: ~p~n", [MaxRecurse]),

    case list_max_errors(List) of
        {ok, MaxErrors} ->
            io:format("Errors(success): ~p~n", [MaxErrors]);
        {error, Message} ->
            io:format("Errors(failure): ~p~n", [Message])
    end,

    handle_max_errors(
      list_max_errors(List)),
    handle_max_errors(
      list_max_errors({hello, world})).

list_max_fold(List) ->
    lists:foldl(fun(Item, Max) ->
                        if
                            Item > Max -> Item;
                            true -> Max
                        end
                end, 0, List).


list_max_recurse(List) ->
    list_max_recurse(List, 0).

list_max_recurse([], Max) ->
    Max;
list_max_recurse([Item|Rest], Max)
  when Item > Max ->
    list_max_recurse(Rest, Item);
list_max_recurse([_Item|Rest], Max) ->
    list_max_recurse(Rest, Max).


list_max_errors(List)
  when is_list(List) ->
    {ok, list_max_recurse(List, 0)};
list_max_errors(_List) ->
    {error, not_a_list}.

handle_max_errors({ok, MaxErrors}) ->
    io:format("Errors(success): ~p~n", [MaxErrors]);
handle_max_errors({error, Message}) ->
    io:format("Errors(failure): ~p~n", [Message]).
