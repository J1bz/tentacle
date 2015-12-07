-module(chat_server).
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    spawn(fun() ->
            start_server(Port),
            common:sleep(infinity)
          end).

start_server(Port) ->
    {ok, ListeningSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    register(server_pid, spawn(fun() -> server([]) end)),
    spawn(fun() -> user_connect(ListeningSocket) end).

server(Users) ->
    io:format("Connected users: ~p~n", [Users]),
    receive
        {connect, Socket} ->
            New_Users = add_user(Socket, Users),
            server(New_Users);
        {disconnect, Socket} ->
            New_Users = rm_user(Socket, Users),
            server(New_Users);
        {message, Socket, Data} ->
            broadcast(Socket, Data, Users),
            server(Users);
        stop ->
            true
    end. 

add_user(Socket, Users) ->
    case inet:peername(Socket) of
        {ok, {Address, Port}} ->
            Str_Address = inet_parse:ntoa(Address),
            Name = {Str_Address, Port},
            case lists:keymember(Name, 2, Users) of
                true ->
                    Users;
                false ->
                    Formatted_Name = common:format("~s:~B", [Str_Address, Port]),
                    common:map(fun(User) -> notify_presence(Formatted_Name, User) end, Users),
                    [{Socket, Name} | Users]
            end;

        {error, Message} ->
            io:format("Error ~p~n", [Message])
    end.

notify_presence(Formatted_Name, User) ->
    % TODO: comment selectionner seulement le premier element d un tuple ?
    {Notified_Socket, _} = User,
    gen_tcp:send(Notified_Socket, common:format("Presence~n~p", [Formatted_Name])).

rm_user(Socket, Users) ->
    % case au cas ou le serveur a recu une mauvaise info
    case lists:keymember(Socket, 1, Users) of
        true ->
            Name = common:get_value(Socket, Users),
            {Ip, Port} = Name,
            Formatted_Name = common:format("~p:~p", [Ip, Port]),

            New_Users = lists:keydelete(Socket, 1, Users),

            common:map(fun(User) -> notify_absence(Formatted_Name, User) end, New_Users),
            New_Users;
        false ->
            Users
    end.

notify_absence(Formatted_Name, User) ->
    {Notified_Socket, _} = User,
    gen_tcp:send(Notified_Socket, common:format("Absence~n~p", [Formatted_Name])).

get_sockets(Users)                   -> get_sockets(Users, []).
get_sockets([User | Users], Sockets) ->
    {Current_Socket, _} = User,
    get_sockets(Users, [Current_Socket | Sockets]);
get_sockets([], Sockets)             -> Sockets.

broadcast(From_Socket, Data, Users) ->
    Sockets = get_sockets(Users),
    common:map_except(fun(Socket) ->
                gen_tcp:send(Socket, Data)
               end, Sockets, From_Socket).

user_connect(ListeningSocket) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(fun() -> user_connect(ListeningSocket) end),

    case inet:peername(Socket) of
        {ok, {Address, Port}} ->
            Str_Address = inet_parse:ntoa(Address),
            io:format("Client connected with addr ~s and port ~B~n", [Str_Address, Port]),
            server_pid ! {connect, Socket},
            listen_user_socket(Socket);
        {error, Message} ->
            io:format("Error: ~p~n", [Message])
    end.

listen_user_socket(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            server_pid ! {message, Socket, Data},
            io:format("Received: ~s~n", [Data]),
            listen_user_socket(Socket);
        {error, closed} ->
            io:format("Connection closed~n"),
            server_pid ! {disconnect, Socket},
            ok
    end.
