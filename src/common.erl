-module(common).

-export([sleep/1, map/2, map_except/3, format/2, socket_to_name/1, socket_to_string/1, get_value/2,get_key/2]).

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

socket_to_name(Socket) ->
    case inet:peername(Socket) of
        {ok, {Address, Port}} ->
            Str_Address = inet_parse:ntoa(Address),
            {ok, {Str_Address, Port}};
        {error, Message} ->
            {error, Message}
    end.

socket_to_string(Socket) ->
    case socket_to_name(Socket) of
        {ok, {Str_Address, Port}} ->
            Socket_String = format("~s:~B", [Str_Address, Port]),
            {ok, Socket_String};
        {error, Message} ->
            {error, Message}
    end.

get_value(_, []) ->
    none;
get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | T]) ->
    get_value(Key, T).

get_key(_, []) ->
    none;
get_key(Value, [{Key, Value} | _]) ->
    Key;
get_key(Value, [_ | T]) ->
    get_key(Value, T).
