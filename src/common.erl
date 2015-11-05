-module(common).

-export([sleep/1, map/2, map_except/3, format/2, get_value/2]).

sleep(T) ->
    receive
        after T ->
            true
        end.

map(F, [H|T]) -> [F(H), map(F, T)];
map(_, [])    -> [].

map_except(F, [H|T], E) ->
    case E == H of
        true ->
            [pass, map_except(F, T, E)];
        false ->
            [F(H), map_except(F, T, E)]
    end;
map_except(_, [], _)       -> [].

format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

get_value(_, []) ->
    none;
get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | T]) ->
    get_value(Key, T).
