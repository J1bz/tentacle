-module(chat_client).
-export([start/2, start/3]).
-export([send/1, send/2]).
-export([disconnect/0]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

%% ----------------------------------
%% @doc
%% Entry point for client core.
%% Str_Address: Server ipv4 address as a string
%% Port: Server port as an integer between 1 and 65535
%% Log_File: optional string argument refering to file where to write debug
%%           logs
%% @end
%% ----------------------------------
start(Str_Address, Port) ->
    start(Str_Address, Port, none).
start(Str_Address, Port, Log_File) ->
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
            start_client(Str_Address, Port),
            common:sleep(infinity)
          end).

%% ----------------------------------
%% @doc
%% Creates client events-handling main loop registering it as client_pid. Also
%% launches a loop listening to server received messages.
%% @end
%% ----------------------------------
start_client(Str_Address, Port) ->
    case inet:parse_address(Str_Address) of
        {ok, Address} ->
            case gen_tcp:connect(Address, Port, ?TCP_OPTIONS, 5000) of
                {ok, Socket} ->
                    error_logger:info_msg("Connection established~n"),
                    register(client_pid, spawn(fun() -> client(Socket) end)),
                    spawn(fun() -> listen_server_socket(Socket) end);
                {error, Error} ->
                    error_logger:error_msg("Error ~s~n", [Error])
            end;
        {error, Error} ->
            error_logger:error_msg("Error ~s~n", [Error])
    end.

%% ----------------------------------
%% @doc
%% Main events loop. Inter-process messages are received and trigger
%% event-specific functions.
%% @end
%% ----------------------------------
client(Socket) ->
    receive
        {send, Message} ->
            gen_tcp:send(Socket, common:format("Data~n~s~n~n", [Message])),
            client(Socket);
        {send, Message, To_Name} ->
            gen_tcp:send(Socket, common:format("Data~n~s~n~s~n",
                                               [Message, To_Name])),
            client(Socket);
        {received, From_Name, Message} ->
            chat_client_ui:notify_data(From_Name, Message),
            client(Socket);
        {presence, Name} ->
            chat_client_ui:notify_presence(Name),
            client(Socket);
        {absence, Name} ->
            chat_client_ui:notify_absence(Name),
            client(Socket);
        {disconnect} ->
            error_logger:info_msg("Disconnecting...~n"),
            gen_tcp:close(Socket);
        stop ->
            true
    end.

%% ----------------------------------
%% @doc
%% External entry point to broadcast messages to server. It should be called by
%% chat_client_ui.
%% @end
%% ----------------------------------
send(Message) ->
    client_pid ! {send, Message},
    ok.

%% ----------------------------------
%% @doc
%% External entry point to send messages to server. It should be called by
%% chat_client_ui.
%% @end
%% ----------------------------------
send(Message, To_Name) ->
    client_pid ! {send, Message, To_Name},
    ok.

%% ----------------------------------
%% @doc
%% External entry point to disconnect from server. It should be called by
%% chat_client_ui.
%% @end
%% ----------------------------------
disconnect() ->
    client_pid ! {disconnect},
    ok.

%% ----------------------------------
%% @doc
%% frame_factory is a kind of finished states machine to parse received frames
%% @end
%% ----------------------------------
frame_factory() ->
    receive
        "Data" ->
            error_logger:info_msg("Frame factory detected a data frame~n"),
            frame_factory(data);
        "Presence" ->
            error_logger:info_msg("Frame factory detected a presence frame~n"),
            frame_factory(presence);
        "Absence" ->
            error_logger:info_msg("Frame factory detected an absence frame~n"),
            frame_factory(absence);
        Other ->
            error_logger:info_msg("Frame factory detected ~p : ignoring...~n",
                                  [Other]),
            frame_factory()
    end.
frame_factory(data) ->
    receive
        Message ->
            error_logger:info_msg("Frame factory detected ~p as a data "
                                  "message~n", [Message]),
            frame_factory(data, Message)
    end;
frame_factory(presence) ->
    receive
        Name ->
            error_logger:info_msg("Frame factory detected that ~p connected~n",
                                  [Name]),
            client_pid ! {presence, Name},
            frame_factory()
    end;
frame_factory(absence) ->
    receive
        Name ->
            error_logger:info_msg("Frame factory detected that ~p "
                                  "disconnected~n", [Name]),
            client_pid ! {absence, Name},
            frame_factory()
    end.
frame_factory(data, Message) ->
    receive
        From_Name ->
            error_logger:info_msg("Frame factory detected that ~p comes from "
                                  "user ~p~n", [Message, From_Name]),
            client_pid ! {received, From_Name, Message},
            frame_factory()
    end.

%% ----------------------------------
%% @doc
%% Listening loop for server-side reception frames. It should be called only
%% with a gen_tcp socket argument. It will itself ``instanciate'' a
%% frame_factory.
%% @end
%% ----------------------------------
listen_server_socket(Socket) ->
    Frame_Factory = spawn(fun() -> frame_factory() end),
    listen_server_socket(Frame_Factory, Socket).
listen_server_socket(Frame_Factory, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            error_logger:info_msg("Received ~p~n", [Bytes]),
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
            listen_server_socket(Frame_Factory, Socket);
        {error, closed} ->
            error_logger:info_msg("Connection lost~n"),
            ok
    end.
