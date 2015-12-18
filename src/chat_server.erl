-module(chat_server).
-export([start/1, start/2]).
-export([kick/1]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

%% ----------------------------------
%% @doc
%% Entry point for server core.
%% Port: Server port as an integer between 1 and 65535. It should not be used,
%%       and you should check if you have rights to use this port (1-1024).
%% Log_File: optional string argument refering to file where to write debug
%%           logs
%% @end
%% ----------------------------------
start(Port) ->
    start(Port, none).
start(Port, Log_File) ->
    error_logger:tty(false),
    case Log_File of
        none ->
            io:format("No logfile has been supplied : debug logs will be "
                      "ignored~n");
        File ->
            case error_logger:logfile({open, File}) of
                ok ->
                    io:format("Logging debug informations in ~p~n", [File]);
                {error, Error} ->
                    io:format("Error opening ~p: ~p~n", [File, Error]),
                    io:format("Debug logs will be ignored~n")
            end
    end,
    spawn(fun() ->
            start_server(Port),
            common:sleep(infinity)
          end).

%% ----------------------------------
%% @doc
%% Creates server events-handling main loop registering it as server_pid. Also
%% launches a loop listening to clients connections.
%% @end
%% ----------------------------------
start_server(Port) ->
    {ok, ListeningSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    register(server_pid, spawn(fun() -> server() end)),
    spawn(fun() -> user_connect(ListeningSocket) end).

%% ----------------------------------
%% @doc
%% Main events loop. Inter-process messages are received and trigger
%% event-specific functions.
%% @end
%% ----------------------------------
server() ->
    error_logger:info_msg("Server main process started~n"),
    server([]).
server(Users) ->
    receive
        {connect, Socket} ->
            New_Users = add_user(Socket, Users),
            server(New_Users);
        {disconnect, Socket} ->
            New_Users = rm_user(Socket, Users),
            server(New_Users);
        {kick, Name} ->
            New_Users = kick_user(Name, Users),
            server(New_Users);
        {broadcast, Socket, Frame} ->
            broadcast(Socket, Frame, Users),
            server(Users);
        {message, Socket, Frame, To_Name} ->
            case common:get_key(To_Name, Users) of
                none ->
                    error_logger:error_msg("Received a message to trasmit to "
                                           "~p but it has not been found in "
                                           "the connected users~n", [To_Name]);
                To_Socket ->
                    message(Socket, Frame, To_Socket)
            end,
            server(Users);
        stop ->
            true
    end.

%% ----------------------------------
%% @doc
%% Handle operation to add an user to connected users and refuse it if an
%% error occurs.
%% @end
%% ----------------------------------
add_user(Connecting_Socket, Connected_Users) ->
    case common:socket_to_name(Connecting_Socket) of
        {ok, Connecting_Name} ->
            case lists:keymember(Connecting_Name, 2, Connected_Users) of
                true ->
                    error_logger:warning_msg("User ~p wanted to connect to "
                                             "server, but it is already "
                                             "connected~n", [Connecting_Name]),
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
                    error_logger:info_msg("User ~p successfully connected to "
                                          "server~n", [Connecting_Name]),
                    New_Users =  [{Connecting_Socket, Connecting_Name} |
                                  Connected_Users],
                    chat_server_ui:notify_presence(Connecting_Name),
                    New_Users
            end;
        {error, Error} ->
            error_logger:error_msg("Error ~p~n", [Error]),
            Connected_Users
    end.

%% ----------------------------------
%% @doc
%% Handle operation to send a presence frame to a socket and log it.
%% @end
%% ----------------------------------
notify_presence(Name, Socket) ->
    Frame = common:format("Presence~n~s~n", [Name]),
    error_logger:info_msg("Sending frame ~p to socket ~p~n", [Frame, Socket]),
    gen_tcp:send(Socket, Frame).

%% ----------------------------------
%% @doc
%% Handle operation to remove an user from connected users or abort it if an
%% error occurs (socket not present in connected users).
%% @end
%% ----------------------------------
rm_user(Socket, Users) ->
    case lists:keymember(Socket, 1, Users) of
        true ->
            Name = common:get_value(Socket, Users),

            error_logger:info_msg("Deleting {~p, ~s} from users list~n",
                                  [Socket, Name]),
            New_Users = lists:keydelete(Socket, 1, Users),
            error_logger:info_msg("Notifying other connected users~n"),
            common:map(fun(User) ->
                        notify_absence(Name, User)
                       end,
                       New_Users),
            error_logger:info_msg("Disconnect operation for user ~p "
                                  "successfully completed~n", [Name]),
            chat_server_ui:notify_absence(Name);
        false ->
            error_logger:error_msg("Socket ~p wanted to disconnect from "
                                   "server, but it is not in users list~n",
                                   [Socket]),
            io:format("Connected users: ~p~n", [Users]),
            Users
    end.

%% ----------------------------------
%% @doc
%% Handle operation to kick an user from connected users or abort it if an
%% error occurs (socket not present in connected users).
%% @end
%% ----------------------------------
kick_user(Name, Users) ->
    case lists:keymember(Name, 2, Users) of
        true ->
            Socket = common:get_key(Name, Users),

            notify_kicked(Name, Socket),

            error_logger:info_msg("Closing socket ~p (~s)~n", [Socket, Name]),
            gen_tcp:close(Socket),

            error_logger:info_msg("Deleting {~p, ~s} from users list~n",
                                  [Socket, Name]),
            New_Users = lists:keydelete(Name, 2, Users),

            common:map(fun(User) ->
                        notify_absence(Name, User)
                       end,
                       New_Users),
            error_logger:info_msg("Kick operation for name ~p "
                                  "successfully completed~n", [Name]),
            chat_server_ui:notify_absence(Name),
            New_Users;
        false ->
            error_logger:error_msg("Kick name from server ~p has been "
                                   "requested, but it is not in users list~n",
                                   [Name]),
            io:format("Connected users: ~p~n", [Users]),
            Users
    end.

%% ----------------------------------
%% @doc
%% Handle operation to send an absence frame to a socket and log it.
%% @end
%% ----------------------------------
notify_absence(Name, User) ->
    {Socket, _} = User,
    Frame = common:format("Absence~n~s~n", [Name]),
    error_logger:info_msg("Sending frame ~p to socket ~p~n", [Frame, Socket]),
    gen_tcp:send(Socket, Frame).

%% ----------------------------------
%% @doc
%% Handle operation to send a data frame informing kicked user that it has
%% been kicked, since gen_tcp:close does not inform peer connected it has been
%% disconnected and log it.
%% @end
%% ----------------------------------
notify_kicked(Name, Socket) ->
    Frame = common:format("Data~nYou have been kicked!~n~s~n", [Name]),
    error_logger:info_msg("Sending frame ~p to socket ~p~n", [Frame, Socket]),
    gen_tcp:send(Socket, Frame).

%% ----------------------------------
%% @doc
%% This function aims to extract from a 2-elements tuple list (like the users
%% list) a list containing only sockets.
%% @end
%% ----------------------------------
get_sockets(Users)                   -> get_sockets(Users, []).
get_sockets([User | Users], Sockets) ->
    {Current_Socket, _} = User,
    get_sockets(Users, [Current_Socket | Sockets]);
get_sockets([], Sockets)             -> Sockets.

%% ----------------------------------
%% @doc
%% Handle operation to send a data frame to a socket and log it.
%% @end
%% ----------------------------------
message(From_Socket, Message, To_Socket) ->
    case common:socket_to_name(From_Socket) of
        {ok, From_Name} ->
            Frame = common:format("Data~n~s~n~s~n", [Message, From_Name]),
            error_logger:info_msg("Sending frame ~p to socket ~p~n",
                                  [Frame, To_Socket]),
            gen_tcp:send(To_Socket, Frame);
        {error, _} ->
            error_logger:error_msg("Wanted to send ~p from ~p to ~p but an "
                                   "error occured during ~p string formattng",
                                   [Message, From_Socket, To_Socket,
                                   From_Socket])
    end.

%% ----------------------------------
%% @doc
%% Handle operation to broadcast a data frame to a users list and log it.
%% @end
%% ----------------------------------
broadcast(From_Socket, Message, Users) ->
    Sockets = get_sockets(Users),
    case common:socket_to_name(From_Socket) of
        {ok, From_Name} ->
            Frame = common:format("Data~n~s~n~s~n", [Message, From_Name]),
            error_logger:info_msg("Broadcasting frame ~p to users ~p~n",
                                  [Frame, Users]),
            common:map_except(fun(Socket) ->
                        gen_tcp:send(Socket, Frame)
                       end, Sockets, From_Socket);
        {error, _} ->
            error_logger:error_msg("Wanted to send ~p from ~p but an error "
                                   "occured while its socket was formatted "
                                   "to string", [Message, From_Socket])
    end.

%% ----------------------------------
%% @doc
%% External entry point to kick a user from server by name. It should be
%% called by chat_client_ui.
%% @end
%% ----------------------------------
kick(Name) ->
    server_pid ! {kick, Name},
    ok.

%% ----------------------------------
%% @doc
%% Listening loop to handle clients connections. If a connection is successful
%% a listen_user_socket loop is ``instantiated''.
%% @end
%% ----------------------------------
user_connect(ListeningSocket) ->
    {ok, Socket} = gen_tcp:accept(ListeningSocket),
    spawn(fun() -> user_connect(ListeningSocket) end),

    case common:socket_to_name(Socket) of
        {ok, Name} ->
            error_logger:info_msg("Socket ~p submitted a connection to "
                                  "server~n", [Socket]),
            server_pid ! {connect, Socket},
            listen_user_socket(Socket, Name);
        {error, Error} ->
            error_logger:error_msg("Error: ~p~n", [Error])
    end.

%% ----------------------------------
%% @doc
%% frame_factory is a kind of finished states machine to parse received frames
%% @end
%% ----------------------------------
frame_factory(Socket) ->
    receive
        "Data" ->
            error_logger:info_msg("~p's frame factory detected a data frame~n",
                                  [Socket]),
            frame_factory(Socket, data);
        Other ->
            error_logger:info_msg("~p's frame factory detected ~p : "
                                  "ignoring...~n", [Socket, Other]),
            frame_factory(Socket)
    end.
frame_factory(Socket, data) ->
    receive
        Message ->
            error_logger:info_msg("~p's frame factory detected ~p as a data "
                                  "message~n", [Socket, Message]),
            frame_factory(Socket, data, Message)
    end.
frame_factory(Socket, data, Message) ->
    receive
        "" ->
            error_logger:info_msg("~p's frame factory detected that ~p has to "
                                  "be broadcasted~n", [Socket, Message]),
            server_pid ! {broadcast, Socket, Message},
            frame_factory(Socket);
        To_Name ->
            error_logger:info_msg("~p's frame factory detected that ~p has to "
                                  "be sent to ~p~n",
                                  [Socket, Message, To_Name]),
            server_pid ! {message, Socket, Message, To_Name},
            frame_factory(Socket)
    end.     

%% ----------------------------------
%% @doc
%% Listening loop for client-side reception frames. It should be called only
%% with a gen_tcp socket and a string name argument. It will itself
%% ``instanciate'' a frame_factory.
%% @end
%% ----------------------------------
listen_user_socket(Socket, Name) ->
    Frame_Factory = spawn(fun() -> frame_factory(Socket) end),
    listen_user_socket(Frame_Factory, Socket, Name).
listen_user_socket(Frame_Factory, Socket, Name) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            error_logger:info_msg("Received ~p from socket ~p~n",
                                  [Bytes, Socket]),
            Splitted_Bytes = re:split(Bytes, "\n|\r\n"),
            if
                length(Splitted_Bytes) > 1 ->
                    Splitted_Lines = lists:reverse(tl(lists:reverse(
                        Splitted_Bytes)));
                true ->  % else
                    Splitted_Lines = Splitted_Bytes
            end,
            common:map(fun(Bytes_Line) ->
                        error_logger:info_msg("Sending ~p to frame factory~n",
                                              [Bytes_Line]),
                        Frame_Factory ! binary_to_list(Bytes_Line)
                       end, Splitted_Lines),
            listen_user_socket(Frame_Factory, Socket, Name);
        {error, closed} ->
            error_logger:info_msg("User ~p closed his socket connection~n",
                                  [Name]),
            server_pid ! {disconnect, Socket},
            ok
    end.
