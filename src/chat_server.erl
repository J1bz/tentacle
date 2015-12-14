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
    register(server_pid, spawn(fun() -> server() end)),
    spawn(fun() -> user_connect(ListeningSocket) end).

server() ->
    io:format("Server main process started~n"),
    server([]).
server(Users) ->
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
                    io:format("User ~p wanted to connect to server, but it is "
                              "already connected~n", [Connecting_Name]),
                    io:format("Connected users: ~p~n", [Connected_Users]),
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
                    io:format("User ~p successfully connected to server~n",
                              [Connecting_Name]),
                    New_Users =  [{Connecting_Socket, Connecting_Name} |
                                  Connected_Users],
                    io:format("Connected users: ~p~n", [New_Users]),
                    New_Users
            end;
        {error, Error} ->
            io:format("Error ~p~n", [Error])
    end.

notify_presence(Name, Socket) ->
    Frame = common:format("Presence~n~s~n", [Name]),
    io:format("Sending frame ~p to socket ~p~n", [Frame, Socket]),
    gen_tcp:send(Socket, Frame).

rm_user(Socket, Users) ->
    % case au cas ou le serveur a recu une mauvaise info
    case lists:keymember(Socket, 1, Users) of
        true ->
            Name = common:get_value(Socket, Users),

            io:format("Deleting {~p, ~s} from users list~n", [Socket, Name]),
            New_Users = lists:keydelete(Socket, 1, Users),
            io:format("Notifying other connected users~n"),
            common:map(fun(User) ->
                        notify_absence(Name, User)
                       end,
                       New_Users),
            io:format("Disconnect operation for user ~p successfully "
                      "completed~n", [Name]),
            io:format("Connected users: ~p~n", [New_Users]),
            New_Users;
        false ->
            io:format("Socket ~p wanted to disconnect from server, but "
                      "it is not in users list~n", [Socket]),
            io:format("Connected users: ~p~n", [Users]),
            Users
    end.

notify_absence(Name, User) ->
    {Socket, _} = User,
    Frame = common:format("Absence~n~s~n", [Name]),
    io:format("Sending frame ~p to socket ~p~n", [Frame, Socket]),
    gen_tcp:send(Socket, Frame).

get_sockets(Users)                   -> get_sockets(Users, []).
get_sockets([User | Users], Sockets) ->
    {Current_Socket, _} = User,
    get_sockets(Users, [Current_Socket | Sockets]);
get_sockets([], Sockets)             -> Sockets.

message(From_Socket, Message, To_Socket) ->
    case common:socket_to_name(From_Socket) of
        {ok, From_Name} ->
            Frame = common:format("Data~n~s~n~s~n", [Message, From_Name]),
            io:format("Sending frame ~p to socket ~p~n", [Frame, To_Socket]),
            gen_tcp:send(To_Socket, Frame);
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
            io:format("Broadcasting frame ~p to users ~p~n", [Frame, Users]),
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
            io:format("Socket ~p submitted a connection to server~n",
                      [Socket]),
            server_pid ! {connect, Socket},
            listen_user_socket(Socket, Name);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end.

frame_factory(Socket) ->
    receive
        "Data" ->
            io:format("~p's frame factory detected a data frame~n", [Socket]),
            frame_factory(Socket, data);
        Other ->
            io:format("~p's frame factory detected ~p : ignoring...~n",
                      [Socket, Other]),
            frame_factory(Socket)
    end.
frame_factory(Socket, data) ->
    receive
        Message ->
            io:format("~p's frame factory detected ~p as a data message~n",
                      [Socket, Message]),
            frame_factory(Socket, data, Message)
    end.
frame_factory(Socket, data, Message) ->
    receive
        "" ->
            io:format("~p's frame factory detected that ~p has to be "
                      "broadcasted~n", [Socket, Message]),
            server_pid ! {broadcast, Socket, Message},
            frame_factory(Socket);
        To_Name ->
            io:format("~p's frame factory detected that ~p has to be "
                      "sent to ~p~n", [Socket, Message, To_Name]),
            server_pid ! {message, Socket, Message, To_Name},
            frame_factory(Socket)
    end.     

listen_user_socket(Socket, Name) ->
    Frame_Factory = spawn(fun() -> frame_factory(Socket) end),
    listen_user_socket(Frame_Factory, Socket, Name).
listen_user_socket(Frame_Factory, Socket, Name) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            io:format("Received ~p from socket ~p~n", [Bytes, Socket]),
            Splitted_Bytes = re:split(Bytes, "\n|\r\n"),
            if
                length(Splitted_Bytes) > 1 ->
                    Splitted_Lines = lists:reverse(tl(lists:reverse(
                        Splitted_Bytes)));
                true ->  % else
                    Splitted_Lines = Splitted_Bytes
            end,
            common:map(fun(Bytes_Line) ->
                        io:format("Sending ~p to frame factory~n", [Bytes_Line]),
                        Frame_Factory ! binary_to_list(Bytes_Line)
                       end, Splitted_Lines),
            listen_user_socket(Frame_Factory, Socket, Name);
        {error, closed} ->
            io:format("User ~p closed his socket connection~n", [Name]),
            server_pid ! {disconnect, Socket},
            ok
    end.
