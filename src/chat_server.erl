-module(chat_server).
-export([start/1]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

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
        {broadcast, Socket, Frame} ->
            broadcast(Socket, Frame, Users),
            server(Users);
        {message, Socket, Frame, To_Name} ->
            case common:get_key(To_Name, Users) of
                none ->
                    io:format("Received a message to trasmit to ~p but it has "
                              "not been found in the connected users~n",
                              [To_Name]);
                To_Socket ->
                    message(Socket, Frame, To_Socket)
            end,
            server(Users);
        stop ->
            true
    end.

add_user(Connecting_Socket, Connected_Users) ->
    case common:socket_to_name(Connecting_Socket) of
        {ok, Connecting_Name} ->
            case lists:keymember(Connecting_Name, 2, Connected_Users) of
                true ->
                    Connected_Users;
                false ->
                    common:map(fun(Connected_User) ->
                                {Connected_Socket, Connected_Name} =
                                        Connected_User,
                                notify_presence(Connecting_Name,
                                                Connected_Socket),
                                notify_presence(Connected_Name,
                                                Connecting_Socket)
                               end,
                               Connected_Users
                    ),
                    [{Connecting_Socket, Connecting_Name} | Connected_Users]
            end;
        {error, Error} ->
            io:format("Error ~p~n", [Error])
    end.

notify_presence(Name, Socket) ->
    gen_tcp:send(Socket, common:format("Presence~n~s~n", [Name])).

rm_user(Socket, Users) ->
    % case au cas ou le serveur a recu une mauvaise info
    case lists:keymember(Socket, 1, Users) of
        true ->
            Name = common:get_value(Socket, Users),

            New_Users = lists:keydelete(Socket, 1, Users),
            common:map(fun(User) -> notify_absence(Name, User) end, New_Users),
            New_Users;
        false ->
            Users
    end.

notify_absence(Formatted_Name, User) ->
    {Notified_Socket, _} = User,
    gen_tcp:send(Notified_Socket, common:format("Absence~n~s~n",
                                                [Formatted_Name])).

get_sockets(Users)                   -> get_sockets(Users, []).
get_sockets([User | Users], Sockets) ->
    {Current_Socket, _} = User,
    get_sockets(Users, [Current_Socket | Sockets]);
get_sockets([], Sockets)             -> Sockets.

message(From_Socket, Message, To_Socket) ->
    case common:socket_to_name(From_Socket) of
        {ok, From_Name} ->
            gen_tcp:send(To_Socket, common:format("Data~n~s~n~s~n",
                                                  [Message, From_Name]));
        {error, _} ->
            io:format("Wanted to send ~p from ~p to ~p but an error occured "
                      "during ~p string formattng",
                      [Message, From_Socket, To_Socket, From_Socket])
    end.

broadcast(From_Socket, Message, Users) ->
    Sockets = get_sockets(Users),
    case common:socket_to_name(From_Socket) of
        {ok, From_Name} ->
            Frame = common:format("Data~n~s~n~s~n", [Message, From_Name]),
            common:map_except(fun(Socket) ->
                        gen_tcp:send(Socket, Frame)
                       end, Sockets, From_Socket);
        {error, _} ->
            io:format("Wanted to send ~p from ~p but an error occured while "
                      "its socket was formatted to string",
                      [Message, From_Socket])
    end.

user_connect(ListeningSocket) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(fun() -> user_connect(ListeningSocket) end),

    case common:socket_to_name(Socket) of
        {ok, Name} ->
            io:format("Client ~s connected~n", [Name]),
            server_pid ! {connect, Socket},
            listen_user_socket(Socket);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end.

listen_user_socket(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Frame} ->
            case common:parse_frame(Frame) of
                {data, {Message}} ->
                    server_pid ! {broadcast, Socket, Message};
                {data, {To_String, Message}} ->
                    server_pid ! {message, Socket, Message, To_String};
                {presence, Socket_String} ->
                    io:format("Received a presence from ~s, but server is not "
                              "supposed to receive this frame... ignoring~n",
                              [Socket_String]);
                {absence, Socket_String} ->
                    io:format("Received an absence from ~s, but server is not "
                              "supposed to receive this frame... ignoring~n",
                              [Socket_String]);
                {unkown, Frame} ->
                    io:format("Received an unkown frame: ~s~n", [Frame])
            end,
            listen_user_socket(Socket);
        {error, closed} ->
            io:format("Connection closed~n"),
            server_pid ! {disconnect, Socket},
            ok
    end.
