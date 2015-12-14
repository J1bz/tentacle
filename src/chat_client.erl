-module(chat_client).
-export([start/2]).
-export([send/1, send/2]).
-export([disconnect/0]).

-define(TCP_OPTIONS, [binary,
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]).

start(Address, Port) ->
    spawn(fun() ->
            start_client(Address, Port),
            common:sleep(infinity)
          end).

start_client(Str_Address, Port) ->
    case inet:parse_address(Str_Address) of
        {ok, Parsed_Address} ->
            case gen_tcp:connect(Parsed_Address, Port, ?TCP_OPTIONS, 5000) of
                {ok, Socket} ->
                    io:format("Connection established~n"),
                    register(client_pid, spawn(fun() -> client(Socket) end)),
                    spawn(fun() -> listen_server_socket(Socket) end);
                {error, Error} ->
                    io:format("Error ~s~n", [Error])
            end;
        {error, Error} ->
            io:format("Error ~s~n", [Error])
    end.

client(Socket) ->
    receive
        {send, Message} ->
            print_send(Message),
            gen_tcp:send(Socket, common:format("Data~n~s~n~n", [Message])),
            client(Socket);
        {send, Message, To_String} ->
            print_send_to(Message, To_String),
            gen_tcp:send(Socket, common:format("Data~n~s~n~s~n",
                                               [Message, To_String])),
            client(Socket);
        {received, From_Name, Message} ->
            print_data(From_Name, Message),
            client(Socket);
        {presence, Name} ->
            print_presence(Name),
            client(Socket);
        {absence, Name} ->
            print_absence(Name),
            client(Socket);
        {disconnect} ->
            io:format("Disconnecting...~n"),
            gen_tcp:close(Socket);
        stop ->
            true
    end.

print_send(Message) ->
    io:format("Me: ~s~n", [Message]).

print_send_to(Message, To_String) ->
    io:format("Me to ~s: ~s~n", [To_String, Message]).

print_data(From_String, Message) ->
    io:format("~s: ~s~n", [From_String, Message]).

print_presence(Socket_String) ->
    io:format("~s joined server~n", [Socket_String]).

print_absence(Socket_String) ->
    io:format("~s left server~n", [Socket_String]).

send(Message) ->
    client_pid ! {send, Message},
    ok.

send(Message, To_String) ->
    client_pid ! {send, Message, To_String},
    ok.

disconnect() ->
    client_pid ! {disconnect},
    ok.

frame_factory() ->
    receive
        "Data" ->
            io:format("Frame factory detected a data frame~n"),
            frame_factory(data);
        "Presence" ->
            io:format("Frame factory detected a presence frame~n"),
            frame_factory(presence);
        "Absence" ->
            io:format("Frame factory detected an absence frame~n"),
            frame_factory(absence);
        Other ->
            io:format("Frame factory detected ~p : ignoring...~n", [Other]),
            frame_factory()
    end.
frame_factory(data) ->
    receive
        String ->
            io:format("Frame factory detected ~p as a data message~n",
                      [String]),
            frame_factory(data, String)
    end;
frame_factory(presence) ->
    receive
        String ->
            io:format("Frame factory detected that ~p connected~n", [String]),
            client_pid ! {presence, String},
            frame_factory()
    end;
frame_factory(absence) ->
    receive
        String ->
            io:format("Frame factory detected that ~p disconnected~n",
                      [String]),
            client_pid ! {absence, String},
            frame_factory()
    end.
frame_factory(data, Message) ->
    receive
        String ->
            io:format("Frame factory detected that ~p comes from user ~p~n",
                      [Message, String]),
            client_pid ! {received, String, Message},
            frame_factory()
    end.

listen_server_socket(Socket) ->
    Frame_Factory = spawn(fun() -> frame_factory() end),
    listen_server_socket(Frame_Factory, Socket).
listen_server_socket(Frame_Factory, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Bytes} ->
            io:format("Received ~p~n", [Bytes]),
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
            listen_server_socket(Frame_Factory, Socket);
        {error, closed} ->
            io:format("Connection lost~n"),
            ok
    end.
