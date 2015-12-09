-module(common).

-export([sleep/1]).
-export([map/2, map_except/3]).
-export([format/2]).
-export([socket_to_name/1]).
-export([parse_frame/1]).
-export([get_value/2, get_key/2]).

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
            Socket_String = format("~s:~B", [Str_Address, Port]),
            {ok, Socket_String};
        {error, Error} ->
            {error, Error}
    end.

parse_frame(Frame) ->
    Splitted_Frame = string:tokens(binary_to_list(Frame), "\n"),
    [Header | Content] = Splitted_Frame,
    case Header of
        "Data" ->
            case length(Content) of
                1 ->
                    [Message] = Content,
                    {data, {Message}};
                2 ->
                    [Message | [Socket_String]] = Content,
                    {data, {Socket_String, Message}}
            end;
        "Presence" ->
            Socket_String = [Content],
            {presence, Socket_String};
        "Absence" ->
            Socket_String = [Content],
            {absence, Socket_String}
        %TODO: le cas ou aucun des trois n'est en jeu
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
