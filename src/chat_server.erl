-module(chat_server).
-export([start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    io:format("new~n"),
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
        {broadcast, Socket, Data} ->
            broadcast(Socket, Data, Users),
            server(Users);
        {message, Socket, Data, To_Name} ->
            case common:get_key(To_Name, Users) of
                none ->
                    io:format("Received a message to trasmit to ~p but it has
not been found in the connected users~n", [To_Name]);
                To_Socket ->
                    message(Socket, Data, To_Socket)
            end,
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
    gen_tcp:send(Notified_Socket, common:format("Presence~n~s~n", [Formatted_Name])).

rm_user(Socket, Users) ->
    % case au cas ou le serveur a recu une mauvaise info
    case lists:keymember(Socket, 1, Users) of
        true ->
            Name = common:get_value(Socket, Users),
            {Ip, Port} = Name,
            Formatted_Name = common:format("~s:~B", [Ip, Port]),

            New_Users = lists:keydelete(Socket, 1, Users),

            common:map(fun(User) -> notify_absence(Formatted_Name, User) end, New_Users),
            New_Users;
        false ->
            Users
    end.

notify_absence(Formatted_Name, User) ->
    {Notified_Socket, _} = User,
    gen_tcp:send(Notified_Socket, common:format("Absence~n~s~n", [Formatted_Name])).

get_sockets(Users)                   -> get_sockets(Users, []).
get_sockets([User | Users], Sockets) ->
    {Current_Socket, _} = User,
    get_sockets(Users, [Current_Socket | Sockets]);
get_sockets([], Sockets)             -> Sockets.

message(From_Socket, Data, To) ->
    case common:socket_to_string(From_Socket) of
        {ok, From_Socket_String} ->
            gen_tcp:send(To, common:format("Data~n~s~n~s~n", [Data, From_Socket_String]));
        {error, _} ->
            io:format("Wanted to send ~p from ~p to ~p but an error occured during ~p string formattng", [Data, From_Socket, To, From_Socket])
    end.

broadcast(From_Socket, Content, Users) ->
    Sockets = get_sockets(Users),
    case common:socket_to_string(From_Socket) of
        {ok, From_Socket_String} ->
            Data = common:format("Data~n~s~n~s~n", [Content, From_Socket_String]),
            common:map_except(fun(Socket) ->
                        gen_tcp:send(Socket, Data)
                       end, Sockets, From_Socket);
        {error, _} ->
            io:format("Wanted to send ~p from ~p but an error occured while its socket was formatted to string", [Content, From_Socket])
    end.

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

parse_name(Name) ->
    Splitted_Name = string:tokens(Name, ":\""),
    case length(Splitted_Name) of
        2 ->
            [Str_Address | [Str_Port]] = Splitted_Name,
            case inet_parse:address(Str_Address) of
                {ok, _} ->
                    %TODO: si le port n'est pas un entier
                    Port = list_to_integer(Str_Port),
                    {ok, {Str_Address, Port}};
                {error, _} ->
                    Reason = common:format("Received ~s as name but ~s address is not valid", [Name, Str_Address]),
                    {error, Reason}
                    end;
        _ ->
            Reason = common:format("Received ~s as name but a name should have only one ':'", [Name]),
            {error, Reason}
    end.

listen_user_socket(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Slitted_Data = string:tokens(binary_to_list(Data), "\n"),
            [Header | Message] = Slitted_Data,
            case Header of
                "Data" ->
                    case length(Slitted_Data) of
                        2 ->
                            server_pid ! {broadcast, Socket, Message};
                        3 ->
                            [Content | [To]] = Message,
                            case parse_name(To) of
                                {ok, {Address, Port}} ->
                                    server_pid ! {message, Socket, Content, {Address, Port}};
                                {error, Message} ->
                                    io:format("Error ~p~n", [Message])
                            end;
                        _ ->
                            io:format("Received an unknown message ~p~n", [Data])
                    end;
                _ ->
                    io:format("Reveiced an unknown message ~p~n", [Data])
            end,
            listen_user_socket(Socket);
        {error, closed} ->
            io:format("Connection closed~n"),
            server_pid ! {disconnect, Socket},
            ok
    end.
