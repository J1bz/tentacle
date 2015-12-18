-module(common).

-export([sleep/1]).
-export([map/2, map_except/3]).
-export([format/2]).
-export([socket_to_name/1]).
-export([get_value/2, get_key/2]).

%% ----------------------------------
%% @doc
%% A simple sleep function taking a number of seconds as argument.
%% @end
%% ----------------------------------
sleep(T) ->
    receive
        after T ->
            true
        end.

%% ----------------------------------
%% @doc
%% map iterates over a supplied list to apply a function on each element.
%% @end
%% ----------------------------------
map(F, [H|T]) -> [F(H), map(F, T)];
map(_, [])    -> [].

%% ----------------------------------
%% @doc
%% map_except iterates over a supplied list to apply a function on each
%% element, except the third parameter.
%% @end
%% ----------------------------------
map_except(F, [H|T], E) ->
    case E == H of
        true ->
            [pass, map_except(F, T, E)];
        false ->
            [F(H), map_except(F, T, E)]
    end;
map_except(_, [], _)       -> [].

%% ----------------------------------
%% @doc
%% Same thing as io:format but returns a string instead of printing it on
%% stdout.
%% @end
%% ----------------------------------
format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

%% ----------------------------------
%% @doc
%% Translate a gen_tcp socket object to a string looking like :
%% "127.0.0.1:45764"
%% @end
%% ----------------------------------
socket_to_name(Socket) ->
    case inet:peername(Socket) of
        {ok, {Address, Port}} ->
            Str_Address = inet_parse:ntoa(Address),
            Socket_String = format("~s:~B", [Str_Address, Port]),
            {ok, Socket_String};
        {error, Error} ->
            {error, Error}
    end.

%% ----------------------------------
%% @doc
%% get_value should be used only on lists of 2-elements tuples (that can be
%% seen as dictionnaries). Key is the first tuple element and Value the
%% second. get_value returns the Value of matching Key in a list, or none.
%% @end
%% ----------------------------------
get_value(_, []) ->
    none;
get_value(Key, [{Key, Value} | _]) ->
    Value;
get_value(Key, [_ | T]) ->
    get_value(Key, T).

%% ----------------------------------
%% @doc
%% get_key should be used only on lists of 2-elements tuples (that can be
%% seen as dictionnaries). Key is the first tuple element and Value the
%% second. get_key returns the Key of matching Value in a list, or none.
%% @end
%% ----------------------------------
get_key(_, []) ->
    none;
get_key(Value, [{Key, Value} | _]) ->
    Key;
get_key(Value, [_ | T]) ->
    get_key(Value, T).
